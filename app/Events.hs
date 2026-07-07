{-# LANGUAGE OverloadedStrings #-}

-- Input handling and state transitions.
module Events (handle) where

import Brick
import Brick.Widgets.List
import Content
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Core
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IORef (IORef, writeIORef)
import Data.List (find, findIndex)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Clock (getMonotonicTime)
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Markdown (siteBase)

-- lastInputRef feeds the tick thread in UI: it pauses fx ticks on idle
-- sessions and enforces the idle/session-cap disconnects (TimeUp).
handle :: IORef Double -> BrickEvent Name Tick -> EventM Name St ()
handle lastInputRef ev = do
  s <- get
  case ev of
    AppEvent Tick -> tick s
    AppEvent TimeUp -> halt
    _ -> liftIO (getMonotonicTime >>= writeIORef lastInputRef)
  -- Latch left-press, clear on release/key; lets the guard below swallow
  -- the drag repeats terminals send while the button is held.
  case ev of
    _ | mouseUp ev -> modify (\st -> st {stMouseHeld = False})
    VtyEvent (V.EvKey _ _) -> modify (\st -> st {stMouseHeld = False})
    _ | leftClick ev -> modify (\st -> st {stMouseHeld = True})
    _ -> pure ()
  case ev of
    AppEvent Tick -> pure ()
    AppEvent TimeUp -> pure ()
    -- Ctrl+C / Ctrl+D quit from anywhere.
    VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
    VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) -> halt
    -- drag repeat while the button is held
    _ | stMouseHeld s, leftClick ev -> pure ()
    -- The "?" overlay swallows the next key/click.
    _ | stHelp s, dismisses ev -> modify (\st -> st {stHelp = False})
    -- Any key/click dismisses the status line; wheel still scrolls.
    _ | stStatus s /= Nothing, dismisses ev -> dismissStatus s ev
    -- While the "/" prompt is open, all keys go to it.
    VtyEvent (V.EvKey k []) | Just buf <- stPrompt s -> promptKey s buf k
    MouseDown n V.BLeft _ (Location (c, r)) -> clickOn n c r
    _ | Just d <- scrollDelta ev -> wheel (stView s) d
    -- brick only turns a click into a MouseDown event when the TOPMOST
    -- extent at that spot is clickable; anything under a viewport gets
    -- shadowed and arrives as a raw vty event instead. Hit-test the
    -- extents ourselves so links and post rows work.
    VtyEvent (V.EvMouseDown c r V.BLeft _) -> do
      exts <- findClickedExtents (c, r)
      case [e | e@(Extent nm _ _) <- exts, actionable nm] of
        Extent nm (Location (ec, er)) _ : _ -> clickOn nm (c - ec) (r - er)
        [] -> pure ()
    -- Mouse releases and stray buttons must not reach the key handler.
    VtyEvent (V.EvMouseUp _ _ _) -> pure ()
    VtyEvent (V.EvMouseDown _ _ _ _) -> pure ()
    VtyEvent vev -> do
      case hintIdxFor (stView s) vev of
        Just i -> setBurst (BHint i)
        Nothing -> pure ()
      key s vev
    _ -> pure ()
  -- reader progress for the bottom bar, from the last render's viewport
  view' <- stView <$> get
  case view' of
    PostView _ -> do
      mvp <- lookupViewport ReaderVP
      modify (\st -> st {stProgress = fmap pctOf mvp})
    _ -> modify (\st -> st {stProgress = Nothing})
  where
    -- distance scrolled through the scrollable range: 0% at the top,
    -- 100% once the last line is on screen
    pctOf vp =
      let (_, vh) = vp ^. vpSize
          (_, ch) = vp ^. vpContentSize
       in if ch <= vh
            then 100
            else min 100 (round (100 * fromIntegral (vp ^. vpTop) / (fromIntegral (ch - vh) :: Double)))

-- wheel scroll from either the brick or raw-vty mouse encoding
scrollDelta :: BrickEvent Name Tick -> Maybe Int
scrollDelta ev = case ev of
  MouseDown _ V.BScrollDown _ _ -> Just wheelStep
  MouseDown _ V.BScrollUp _ _ -> Just (-wheelStep)
  VtyEvent (V.EvMouseDown _ _ V.BScrollDown _) -> Just wheelStep
  VtyEvent (V.EvMouseDown _ _ V.BScrollUp _) -> Just (-wheelStep)
  _ -> Nothing

leftClick :: BrickEvent Name Tick -> Bool
leftClick ev = case ev of
  MouseDown _ V.BLeft _ _ -> True
  VtyEvent (V.EvMouseDown _ _ V.BLeft _) -> True
  _ -> False

mouseUp :: BrickEvent Name Tick -> Bool
mouseUp ev = case ev of
  MouseUp {} -> True
  VtyEvent (V.EvMouseUp {}) -> True
  _ -> False

dismisses :: BrickEvent Name Tick -> Bool
dismisses ev = case ev of
  VtyEvent (V.EvKey _ _) -> True
  _ -> leftClick ev || scrollDelta ev /= Nothing

dismissStatus :: St -> BrickEvent Name Tick -> EventM Name St ()
dismissStatus s ev = do
  modify (\st -> st {stStatus = Nothing})
  maybe (pure ()) (wheel (stView s)) (scrollDelta ev)

promptKey :: St -> Text -> V.Key -> EventM Name St ()
promptKey s buf k = case k of
  V.KEsc -> modify (\st -> st {stPrompt = Nothing})
  V.KEnter ->
    let q = if T.null buf then Nothing else Just (buf, 0)
     in do
          modify (\st -> st {stPrompt = Nothing, stQuery = q, stPing = True})
          case (stView s, q) of
            (PageView ThoughtsTab, Just (qq, _)) -> listSearch qq 1
            _ -> pure ()
  V.KBS -> modify (\st -> st {stPrompt = Just (T.dropEnd 1 buf)})
  V.KChar c -> modify (\st -> st {stPrompt = Just (T.snoc buf c)})
  _ -> pure ()

openPrompt :: EventM Name St ()
openPrompt = modify (\s -> s {stPrompt = Just ""})

bumpHit :: Int -> EventM Name St ()
bumpHit d = modify (\s -> s {stQuery = fmap (fmap (+ d)) (stQuery s), stPing = True})

-- vi-ish: first esc clears an active search, second does the view's esc.
escOr :: St -> EventM Name St () -> EventM Name St ()
escOr s act
  | stQuery s /= Nothing = modify (\st -> st {stQuery = Nothing})
  | otherwise = act

-- Move the list selection to the next/previous matching post.
listSearch :: Text -> Int -> EventM Name St ()
listSearch q dir = do
  s <- get
  let n = length allPosts
      cur = maybe 0 id (listSelected (stList s))
      ql = T.toLower q
      matchP p =
        any (T.isInfixOf ql . T.toLower) (postTitle p : postTags p <> maybe [] (: []) (postDesc p))
      found = find (\j -> matchP (allPosts !! j)) [(cur + dir * step) `mod` n | step <- [1 .. n]]
  when (n > 0) $ case found of
    Just j -> listNav (zoom listL (modify (listMoveTo j)))
    Nothing -> pure ()

actionable :: Name -> Bool
actionable nm = case nm of
  LogoField -> True
  LinkTo _ -> True
  PostRow _ -> True
  TabBtn _ -> True
  BrandBtn -> True
  _ -> False

clickOn :: Name -> Int -> Int -> EventM Name St ()
clickOn nm lc lr = case nm of
  -- ponytail: cap at 12 concurrent ripples; a burst clicker can't outrun the prune
  LogoField -> modify (\st -> st {stRipple = take 12 ((lc, lr, stTick st) : stRipple st)})
  LinkTo u -> linkAction u
  PostRow i -> zoom listL (modify (listMoveTo i)) >> openSelected
  TabBtn t -> switchTab t
  BrandBtn -> toLanding
  _ -> pure ()

tick :: St -> EventM Name St ()
tick s =
  modify $ \st ->
    st
      { stTick = stTick s + 1,
        stRipple = filter (\(_, _, t0) -> stTick s - t0 <= rippleFrames) (stRipple s),
        stBurst = case stBurst s of
          Just (_, t0) | stTick s - t0 > burstFrames -> Nothing
          b -> b,
        -- search scroll request applies for one render, then releases
        stPing = False
      }

setBurst :: BurstTarget -> EventM Name St ()
setBurst bt = modify (\s -> s {stBurst = Just (bt, stTick s)})

-- Keys that act the same in every view, then per-view dispatch.
key :: St -> V.Event -> EventM Name St ()
key s vev = case vev of
  V.EvKey (V.KChar '?') [] -> modify (\st -> st {stHelp = True})
  V.EvKey (V.KChar '/') [] | notLanding -> openPrompt
  _ -> viewKey s vev
  where
    notLanding = case stView s of Landing -> False; _ -> True

viewKey :: St -> V.Event -> EventM Name St ()
viewKey s vev = case stView s of
  Landing -> case vev of
    V.EvKey (V.KChar 'q') [] -> halt
    V.EvKey V.KEsc [] -> halt
    V.EvKey V.KEnter [] -> switchTab (stSel s)
    V.EvKey (V.KChar '\t') [] -> select nextTab
    _ | Just t <- tabMove vev (stSel s) -> select (const t)
    _ -> pure ()
  PageView ThoughtsTab -> case vev of
    V.EvKey (V.KChar 'n') [] | Just (q, _) <- stQuery s -> listSearch q 1
    V.EvKey (V.KChar 'N') [] | Just (q, _) <- stQuery s -> listSearch q (-1)
    V.EvKey V.KEnter [] -> openSelected
    V.EvKey (V.KChar 'q') [] -> halt
    V.EvKey V.KEsc [] -> escOr s toLanding
    V.EvKey (V.KChar '\t') [] -> switchTab (nextTab ThoughtsTab)
    _ | Just t <- tabMove vev ThoughtsTab -> switchTab t
    _ -> listNav (zoom listL (handleListEventVi handleListEvent vev))
  PageView t -> case vev of
    V.EvKey (V.KChar 'n') [] | stQuery s /= Nothing -> bumpHit 1
    V.EvKey (V.KChar 'N') [] | stQuery s /= Nothing -> bumpHit (-1)
    V.EvKey (V.KChar 'q') [] -> halt
    V.EvKey V.KEsc [] -> escOr s toLanding
    V.EvKey (V.KChar '\t') [] -> switchTab (nextTab t)
    _ | Just t' <- tabMove vev t -> switchTab t'
    _ -> scrollVp (PageView t) vev
  PostView p -> case vev of
    V.EvKey (V.KChar 'n') [] | stQuery s /= Nothing -> bumpHit 1
    V.EvKey (V.KChar 'N') [] | stQuery s /= Nothing -> bumpHit (-1)
    V.EvKey (V.KChar 'q') [] -> backToList
    V.EvKey V.KEsc [] -> escOr s backToList
    V.EvKey (V.KChar 'h') [] -> backToList
    V.EvKey V.KLeft [] -> backToList
    V.EvKey (V.KChar 'y') [] -> copyUrl (postUrl p)
    V.EvKey (V.KChar 'o') [] -> showStatus (postUrl p)
    _ -> scrollVp (stView s) vev

-- Which bottom-bar hint a keypress corresponds to
hintIdxFor :: View -> V.Event -> Maybe Int
hintIdxFor v (V.EvKey k _) = findIndex (\(ks, _, _) -> k `elem` ks) (hintMap v)
hintIdxFor _ _ = Nothing

-- h/l and left/right switch pages
tabMove :: V.Event -> Tab -> Maybe Tab
tabMove (V.EvKey (V.KChar 'h') []) t = Just (prevTab t)
tabMove (V.EvKey V.KLeft []) t = Just (prevTab t)
tabMove (V.EvKey (V.KChar 'l') []) t = Just (nextTab t)
tabMove (V.EvKey V.KRight []) t = Just (nextTab t)
tabMove _ _ = Nothing

-- Run a list movement; highlight the newly selected title if moved
listNav :: EventM Name St () -> EventM Name St ()
listNav act = do
  before <- (listSelected . stList) <$> get
  act
  after <- (listSelected . stList) <$> get
  when (before /= after) (maybe (pure ()) (setBurst . BPost) after)

-- which scrollable viewport a view owns
viewportFor :: View -> Maybe (ViewportScroll Name)
viewportFor (PageView ThoughtsTab) = Nothing -- the list scrolls itself
viewportFor (PageView t) = Just (viewportScroll (PageVP t))
viewportFor (PostView _) = Just (viewportScroll ReaderVP)
viewportFor Landing = Nothing

wheel :: View -> Int -> EventM Name St ()
wheel v d = case v of
  PageView ThoughtsTab -> listNav (zoom listL (modify (listMoveBy (signum d))))
  _ -> maybe (pure ()) (\vp -> vScrollBy vp d) (viewportFor v)

scrollVp :: View -> V.Event -> EventM Name St ()
scrollVp v ev = maybe (pure ()) (\vp -> scrollKeys vp ev) (viewportFor v)

scrollKeys :: ViewportScroll Name -> V.Event -> EventM Name St ()
scrollKeys vp ev = case ev of
  V.EvKey (V.KChar 'j') [] -> vScrollBy vp 1
  V.EvKey V.KDown [] -> vScrollBy vp 1
  V.EvKey (V.KChar 'k') [] -> vScrollBy vp (-1)
  V.EvKey V.KUp [] -> vScrollBy vp (-1)
  V.EvKey (V.KChar ' ') [] -> vScrollPage vp Down
  V.EvKey V.KPageDown [] -> vScrollPage vp Down
  V.EvKey (V.KChar 'b') [] -> vScrollPage vp Up
  V.EvKey V.KPageUp [] -> vScrollPage vp Up
  V.EvKey (V.KChar 'g') [] -> vScrollToBeginning vp
  V.EvKey (V.KChar 'G') [] -> vScrollToEnd vp
  _ -> pure ()

-- Internal links navigate in-app; everything else displays in the
-- status line (& stays OSC 8 clickable in capable terminals)
linkAction :: Text -> EventM Name St ()
linkAction u
  | Just slug <- T.stripPrefix (siteBase <> "/thoughts/") u = openSlug (T.takeWhile (/= '#') slug)
  | u == siteBase || u == siteBase <> "/" = switchTab HomeTab
  | u == siteBase <> "/thoughts" = switchTab ThoughtsTab
  | u == siteBase <> "/etc" = switchTab EtcTab
  | otherwise = showStatus u

showStatus :: Text -> EventM Name St ()
showStatus u = modify (\s -> s {stStatus = Just u, stBurst = Just (BStatus, stTick s)})

postUrl :: Post -> Text
postUrl p = siteBase <> "/thoughts/" <> postSlug p

-- OSC 52: set the visitor's clipboard through their terminal; widely
-- supported over SSH, silently ignored elsewhere
copyUrl :: Text -> EventM Name St ()
copyUrl u = do
  vty <- getVtyHandle
  liftIO $ V.outputByteBuffer (V.outputIface vty) ("\ESC]52;c;" <> b64 (TE.encodeUtf8 u) <> "\a")
  showStatus ("copied · " <> u)

-- ponytail: hand-rolled base64 beats a new dependency for one OSC 52 payload
b64 :: BS.ByteString -> BS.ByteString
b64 = BC.pack . go . map fromIntegral . BS.unpack
  where
    alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    enc n k = alpha !! ((n `shiftR` k) .&. 63)
    go (a : b : c : r) =
      let n = a `shiftL` 16 .|. b `shiftL` 8 .|. c :: Int
       in enc n 18 : enc n 12 : enc n 6 : enc n 0 : go r
    go [a, b] = let n = a `shiftL` 16 .|. b `shiftL` 8 :: Int in [enc n 18, enc n 12, enc n 6, '=']
    go [a] = let n = a `shiftL` 16 :: Int in [enc n 18, enc n 12, '=', '=']
    go [] = []

openSlug :: Text -> EventM Name St ()
openSlug slug = case find ((== slug) . postSlug) allPosts of
  Just p -> openPost p
  Nothing -> showStatus (siteBase <> "/thoughts/" <> slug)

select :: (Tab -> Tab) -> EventM Name St ()
select f =
  modify (\s -> let t = f (stSel s) in s {stSel = t, stBurst = Just (BTab t, stTick s)})

switchTab :: Tab -> EventM Name St ()
switchTab t =
  modify (\s -> s {stView = PageView t, stSel = t, stBurst = Just (BTab t, stTick s)})

toLanding :: EventM Name St ()
toLanding =
  modify (\s -> s {stView = Landing, stSel = maybe (stSel s) id (currentTab (stView s))})

backToList :: EventM Name St ()
backToList = modify (\s -> s {stView = PageView ThoughtsTab})

openPost :: Post -> EventM Name St ()
openPost p = do
  modify (\st -> st {stView = PostView p, stBurst = Just (BTitle, stTick st)})
  vScrollToBeginning (viewportScroll ReaderVP)

openSelected :: EventM Name St ()
openSelected = do
  s <- get
  case listSelectedElement (stList s) of
    Just (_, p) -> openPost p
    Nothing -> pure ()
