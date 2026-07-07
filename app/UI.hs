{-# LANGUAGE OverloadedStrings #-}

-- Landing + horizontal tabs mirroring the site's pages: home (about),
-- thoughts (post list + reader), etc. The landing state is its own
-- un-selected tab: a glitchy ASCII rendition of the site logo that
-- flickers and drifts toward the tab currently *selected* (h/l moves
-- the selection; enter or a click activates it).
--
-- Keys: h/l/arrows select on landing, switch on pages · 1/2/3 jump ·
-- j/k scroll · enter read/open · esc back · q quit. Mouse: click
-- tabs/posts/links, wheel scrolls.
-- Links: internal ones navigate in-app; external/mailto show the URL
-- in the status line (and are OSC 8 modifier-clickable in capable
-- terminals).
module UI (runTui) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.List
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.State.Class (get, modify)
import Data.Char (isAlphaNum)
import Data.List (find, groupBy, intersperse, nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V (mkVty)
import Lens.Micro (Lens', lens, (^.))
import Lens.Micro.Mtl (zoom)
import System.Environment (lookupEnv)

import Content
import Markdown

data Tab = HomeTab | ThoughtsTab | EtcTab
  deriving (Eq, Ord, Show)

data Name
  = BrandBtn
  | TabBtn Tab
  | LogoField
  | PostList
  | PostRow Int
  | PageVP Tab
  | ReaderVP
  | LinkTo Text
  deriving (Eq, Ord, Show)

data View = Landing | PageView Tab | PostView Post

data Tick = Tick

data St = St
  { stList :: GenericList Name Vec.Vector Post
  , stView :: View
  , stUser :: Maybe Text
  , stTick :: Int
  , stSel :: Tab
  , stEnergy :: Double
  , stRipple :: Maybe (Int, Int, Int)
  , stBurst :: Maybe (BurstTarget, Int)
  , stStatus :: Maybe Text
  , stLastInput :: Int
  }

-- Transient scramble-in effect targets.
data BurstTarget = BTab Tab | BTitle | BStatus | BHint Int | BPost Int
  deriving (Eq)

listL :: Lens' St (GenericList Name Vec.Vector Post)
listL = lens stList (\s l -> s {stList = l})

-- Content column cap so wide terminals read like a page, not a banner.
columnWidth :: Int
columnWidth = 92

runTui :: IO ()
runTui = do
  user <- (>>= sanitizeUser) <$> lookupEnv "USER"
  chan <- newBChan 16
  -- 10fps tick drives the eye; on other views ticks are ignored and
  -- terminal diffing makes the redraw a no-op.
  void . forkIO . forever $ writeBChan chan Tick >> threadDelay 100000
  let buildVty = do
        v <- V.mkVty V.defaultConfig
        let out = V.outputIface v
        V.setMode out V.Mouse True
        when (V.supportsMode out V.Hyperlink) (V.setMode out V.Hyperlink True)
        pure v
  vty <- buildVty
  void $ customMain vty buildVty (Just chan) app (initialSt user)

-- The ssh username is attacker-controlled input: keep only benign
-- characters (no escape sequences) and cap the length.
sanitizeUser :: String -> Maybe Text
sanitizeUser raw
  | T.null t || t == "blog" = Nothing
  | otherwise = Just t
 where
  t = T.take 24 (T.filter (\c -> isAlphaNum c || c `elem` ("-_." :: String)) (T.pack raw))

initialSt :: Maybe Text -> St
initialSt user =
  St (list PostList (Vec.fromList allPosts) 4) Landing user 0 HomeTab 1 Nothing Nothing Nothing 0

app :: App St Tick Name
app =
  App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handle
    , appStartEvent = pure ()
    , appAttrMap = const theMap
    }

titleAttr, metaAttr, activeTabAttr, logoDenseAttr, logoMidAttr, logoHotAttr :: AttrName
titleAttr = attrName "item-title"
metaAttr = attrName "item-meta"
activeTabAttr = attrName "tab-active"
logoDenseAttr = attrName "logo-dense"
logoMidAttr = attrName "logo-mid"
logoHotAttr = attrName "logo-hot"

theMap :: AttrMap
theMap =
  attrMap V.defAttr $
    [ (titleAttr, V.defAttr `V.withStyle` V.bold)
    , (metaAttr, V.defAttr `V.withStyle` V.dim)
    , (activeTabAttr, V.defAttr `V.withForeColor` V.cyan `V.withStyle` V.bold `V.withStyle` V.underline)
    , (listSelectedAttr, V.defAttr `V.withStyle` V.bold)
    , (logoDenseAttr, V.defAttr `V.withForeColor` V.red)
    , (logoMidAttr, V.defAttr `V.withForeColor` V.magenta `V.withStyle` V.dim)
    , (logoHotAttr, V.defAttr `V.withForeColor` V.brightRed `V.withStyle` V.bold)
    ]
      <> markdownAttrs

tabs :: [(Tab, Text)]
tabs = [(HomeTab, "home"), (ThoughtsTab, "thoughts"), (EtcTab, "etc")]

currentTab :: View -> Maybe Tab
currentTab (PageView t) = Just t
currentTab (PostView _) = Just ThoughtsTab
currentTab Landing = Nothing

nextTab :: Tab -> Tab
nextTab HomeTab = ThoughtsTab
nextTab ThoughtsTab = EtcTab
nextTab EtcTab = HomeTab

prevTab :: Tab -> Tab
prevTab = nextTab . nextTab

numTab :: Char -> Maybe Tab
numTab '1' = Just HomeTab
numTab '2' = Just ThoughtsTab
numTab '3' = Just EtcTab
numTab _ = Nothing

-- The logo ------------------------------------------------------------

-- ASCII rasterization of static/assets/logo-standard.png.
logoArt :: [Text]
logoArt =
  [ "+@@@@@@@@@*"
  , " =@@@@@@@@@%."
  , "  :@@@@@@@@@@-"
  , "   .%@@@@@@@@@@@@@@@@@@@@@@@@="
  , "     #@@@@@@@@@@@@@@@@@@@@@@@@*"
  , "      .......-@@@@@@@@@@@@@@@@@%."
  , "       .......:@@@@@@@@@@@@@@@@@@:"
  , "     *@@@@@@@@@*.......:%@@@@@@@@@"
  , "   .%@@@@@@@@@=          *@@@@@@%."
  , "  -@@@@@@@@@@:            +@@@@#"
  , " =@@@@@@@@@%.              -@@*"
  , "+@@@@@@@@@#                 .:    :::::::::."
  , "        :@%.                    .%@@@@@@@@@="
  , "       -@@@@:                  :@@@@@@@@@@-"
  , "      +@@@@@@=                =@@@@@@@@@%."
  , "     #@@@@@@@@*              *@@@@@@@@@#"
  , "     +@@@@@@@@@#-::::::::::-#@@@@@@@@@+"
  , "      -@@@@@@@@=+@@@@@@@@@@@@:"
  , "       .%@@@@@:  -@@@@@@@@@@@@="
  , "        .#@@%.    .%@@@@@@@@@@@*"
  , "          +*        #@@@@@@@@@@@*"
  ]

logoW :: Int
logoW = 44

-- Same mark, hand-drawn, for the page header emblem.
miniLogoArt :: [Text]
miniLogoArt =
  [ "+@@"
  , "  :@@@@="
  , " .-@   @."
  , "+@.    ."
  , " .@@.   .%"
  , "   .@@@@="
  ]

-- Glitchy render: deterministic per-cell noise keyed on the tick swaps
-- characters and occasionally runs a cell hot. Interaction energy
-- raises the scramble rate; a click ripple scrambles an expanding ring
-- and sparks across the empty gaps.
glitchArt :: [Text] -> Int -> Double -> Maybe (Int, Int, Int) -> [[(AttrName, Text)]]
glitchArt art tick energy ripple =
  [ runs [cell x y c | (x, c) <- zip [0 ..] (T.unpack row)]
  | (y, row) <- zip [0 ..] art
  ]
 where
  inRipple x y = case ripple of
    Nothing -> False
    Just (rx, ry, t0) ->
      let age = fromIntegral (tick - t0) :: Double
          dx = fromIntegral (x - rx)
          dy = fromIntegral (y - ry) * 2 -- char cells are ~2:1
          d = sqrt (dx * dx + dy * dy)
       in abs (d - age * 2.2) < 2.2
  cell x y c
    | c == ' ' =
        if inRipple x y && hash x y < 25
          then (logoMidAttr, '·')
          else (metaAttr, ' ')
    | otherwise =
        let h = hash x y
            gp = 6 + round (energy * 28) + (if inRipple x y then 55 else 0)
            c' = if h < gp then glitch !! (h `mod` length glitch) else c
            a
              | inRipple x y && h < 30 = logoHotAttr
              | h < 2 + round (energy * 6) = logoHotAttr
              | c' `elem` ("@%Pqbd█▛▜▙▟▀▄▌▐▘▝▖▗▚▞" :: String) = logoDenseAttr
              | c' `elem` ("#*+=" :: String) = logoMidAttr
              | otherwise = metaAttr
         in (a, c')
  hash x y = (x * 7919 + y * 104729 + (tick `div` 2) * 31337) `mod` 101
  glitch = "%#*=:@" :: String
  runs = map (\g -> (fst (head g), T.pack (map snd g))) . groupBy (\a b -> fst a == fst b)

-- A horizontal rule that drops the occasional stitch; more when the
-- session is energetic.
glitchRule :: Int -> Double -> Widget Name
glitchRule tick energy =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx ^. availWidthL
        ch x =
          let h = (x * 6151 + (tick `div` 4) * 13007) `mod` 211
           in if h < 2 + round (energy * 12)
                then (if even h then '┄' else '╌')
                else '─'
    render (withAttr metaAttr (txt (T.pack (map ch [0 .. w - 1]))))

-- Drawing --------------------------------------------------------------

draw :: St -> [Widget Name]
draw s = case stView s of
  Landing -> [drawLanding s]
  v -> [vBox [topBar s v, body v, bottomBar s v]]
 where
  body (PageView ThoughtsTab) =
    vBox
      [ C.hCenter . hLimit columnWidth . padTop (Pad 1) . padLeftRight 2 $
          vBox
            [ withAttr headingAttr (txt "thoughts")
            , withAttr metaAttr (txtWrap thoughtsSub)
            , txt " "
            ]
      , C.hCenter (hLimit columnWidth (renderListWithIndex (postRow s) True (stList s)))
      ]
  body (PageView t) =
    pageViewport t (contentPage (if t == HomeTab then aboutPage else etcPage))
  body (PostView p) = readerBody s p
  body Landing = emptyWidget

drawLanding :: St -> Widget Name
drawLanding s =
  vBox
    [ C.center . clickable LogoField . hLimit logoW . vBox $
        [ hBox [withAttr a (txt t) | (a, t) <- row]
        | row <- glitchArt logoArt (stTick s) (stEnergy s) (stRipple s)
        ]
    , centeredWrap titleAttr heroHead
    , centeredWrap metaAttr (heroRest <> greeting)
    , txt " "
    , C.hCenter (tabRow s (Just (stSel s)))
    , txt " "
    , centeredWrap metaAttr "h/l select · enter opens · 1/2/3 jump · q quits"
    , txt " "
    ]
 where
  greeting = maybe "" (" · hi, " <>) (stUser s)
  -- The site's actual hero copy (about.md frontmatter).
  (heroHead, heroRest) = case pcTitle aboutPage of
    (hd : rest) -> (hd, T.unwords rest)
    [] -> ("kio.dev", "")

-- Centered line that wraps on narrow terminals with each wrapped line
-- individually centered (txtWrap alone left-aligns inside its box).
centeredWrap :: AttrName -> Text -> Widget Name
centeredWrap a t =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = max 8 (min 64 (ctx ^. availWidthL))
    render . vBox $ [C.hCenter (withAttr a (txt l)) | l <- wrapLines w t]

wrapLines :: Int -> Text -> [Text]
wrapLines w = go . T.words
 where
  go [] = [" "]
  go ws =
    let (line, rest) = fill [] 0 ws
     in T.unwords (Prelude.reverse line) : if null rest then [] else go rest
  fill acc _ [] = (acc, [])
  fill acc len (x : xs)
    | null acc = fill [x] (T.length x) xs
    | len + 1 + T.length x <= w = fill (x : acc) (len + 1 + T.length x) xs
    | otherwise = (acc, x : xs)

tabRow :: St -> Maybe Tab -> Widget Name
tabRow s active =
  hBox . intersperse (withAttr metaAttr (txt "  ·  ")) $
    [ clickable (TabBtn t) $
        case burstAge s (BTab t) of
          Just (age, salt) -> scramble (tabSeed t + salt * 17) age attr label
          Nothing -> withAttr attr (txt label)
    | (t, label) <- tabs
    , let attr = if Just t == active then activeTabAttr else metaAttr
    ]
 where
  tabSeed HomeTab = 1
  tabSeed ThoughtsTab = 2
  tabSeed EtcTab = 3

topBar :: St -> View -> Widget Name
topBar s v =
  vBox
    [ padLeftRight 2 $
        hBox
          [ clickable BrandBtn . hLimit 11 . vBox $
              [ hBox [withAttr a (txt t) | (a, t) <- row]
              | row <- glitchArt miniLogoArt (stTick s) (stEnergy s) Nothing
              ]
          , txt "   "
          , vLimit (length miniLogoArt) . C.vCenter . vBox $
              [ hBox
                  [ clickable BrandBtn (withAttr titleAttr (txt "kio.dev"))
                  , -- distilled from about.md's closing line
                    withAttr metaAttr (txt " · yapping about programming, security, & whatever else")
                  , padLeft Max (withAttr metaAttr (txt (maybe " " ("hi, " <>) (stUser s))))
                  ]
              , txt " "
              , tabRow s (currentTab v)
              ]
          ]
    , glitchRule (stTick s) (stEnergy s)
    ]

-- e.g. "7 thoughts · mostly security, guides, programming"
thoughtsSub :: Text
thoughtsSub =
  T.pack (show (length allPosts))
    <> " thoughts"
    <> (if null topTags then "" else " · mostly " <> T.intercalate ", " topTags)
 where
  allTags = concatMap postTags allPosts
  topTags =
    take 3 . map fst . sortOn (negate . snd) $
      [(t, length (filter (== t) allTags)) | t <- nub allTags]

bottomBar :: St -> View -> Widget Name
bottomBar s v = padLeftRight 2 $ case stStatus s of
  Just u ->
    hBox
      [ withAttr activeTabAttr (txt "→ ")
      , case burstAge s BStatus of
          Just (age, salt) -> scramble (4 + salt * 17) age titleAttr u
          Nothing -> withAttr titleAttr (txt u)
      , padLeft Max (withAttr metaAttr (txt "any key dismisses"))
      ]
  Nothing ->
    padLeft Max . hBox . intersperse (withAttr metaAttr (txt " · ")) $
      [ hBox
          [ -- plain brief highlight; scrambling here proved distracting
            withAttr (maybe metaAttr (const titleAttr) (burstAge s (BHint i))) (txt lbl)
          , withAttr metaAttr (txt (" " <> d))
          ]
      | (i, (lbl, d)) <- zip [0 ..] (hintSegs v)
      ]

hintSegs :: View -> [(Text, Text)]
hintSegs (PageView ThoughtsTab) = [("j/k", "move"), ("enter", "read"), ("h/l", "pages"), ("esc", "landing"), ("q", "quit")]
hintSegs (PostView _) = [("j/k", "scroll"), ("space/b", "page"), ("g/G", "ends"), ("esc", "back")]
hintSegs _ = [("j/k", "scroll"), ("h/l", "pages"), ("esc", "landing"), ("q", "quit")]

-- Which bottom-bar hint a keypress corresponds to, for the burst.
hintIdxFor :: View -> V.Event -> Maybe Int
hintIdxFor v (V.EvKey k _) = case v of
  PageView ThoughtsTab -> case k of
    V.KChar 'j' -> Just 0
    V.KChar 'k' -> Just 0
    V.KUp -> Just 0
    V.KDown -> Just 0
    V.KEnter -> Just 1
    V.KChar 'h' -> Just 2
    V.KChar 'l' -> Just 2
    V.KChar '\t' -> Just 2
    V.KLeft -> Just 2
    V.KRight -> Just 2
    V.KEsc -> Just 3
    _ -> Nothing
  PostView _ -> case k of
    V.KChar 'j' -> Just 0
    V.KChar 'k' -> Just 0
    V.KUp -> Just 0
    V.KDown -> Just 0
    V.KChar ' ' -> Just 1
    V.KChar 'b' -> Just 1
    V.KPageUp -> Just 1
    V.KPageDown -> Just 1
    V.KChar 'g' -> Just 2
    V.KChar 'G' -> Just 2
    V.KEsc -> Just 3
    V.KChar 'h' -> Just 3
    V.KLeft -> Just 3
    _ -> Nothing
  PageView _ -> case k of
    V.KChar 'j' -> Just 0
    V.KChar 'k' -> Just 0
    V.KUp -> Just 0
    V.KDown -> Just 0
    V.KChar 'h' -> Just 1
    V.KChar 'l' -> Just 1
    V.KChar '\t' -> Just 1
    V.KLeft -> Just 1
    V.KRight -> Just 1
    V.KEsc -> Just 2
    _ -> Nothing
  Landing -> Nothing
hintIdxFor _ _ = Nothing

linkify :: Text -> Widget Name -> Widget Name
linkify u = clickable (LinkTo u)

pageViewport :: Tab -> Widget Name -> Widget Name
pageViewport t w =
  viewport (PageVP t) Vertical $
    C.hCenter (hLimit columnWidth (padTopBottom 1 (padLeftRight 2 w)))

-- First title line is the page heading; any remaining lines are subtext.
contentPage :: PageContent -> Widget Name
contentPage pc =
  vBox (titleLines <> [txt " ", renderMarkdown linkify (pcBody pc)])
 where
  titleLines = case pcTitle pc of
    (hd : rest) ->
      withAttr headingAttr (txt hd) : [withAttr metaAttr (txt l) | l <- rest]
    [] -> []

postRow :: St -> Int -> Bool -> Post -> Widget Name
postRow s i sel p =
  clickable (PostRow i) . padLeftRight 2 $
    vBox
      [ hBox
          [ withAttr tAttr (txt (if sel then "❯ " else "  "))
          , case burstAge s (BPost i) of
              Just (age, salt) -> scramble (i * 91 + salt * 17) age tAttr (postTitle p)
              Nothing -> withAttr tAttr (txt (postTitle p))
          ]
      , withAttr metaAttr (txt ("    " <> descLine))
      , withAttr metaAttr (txt ("    " <> postDate p <> tagSuffix p))
      , txt " "
      ]
 where
  tAttr = if sel then activeTabAttr else titleAttr
  descLine = maybe " " (truncate1 84) (postDesc p)
  truncate1 n t =
    let t' = T.unwords (T.words t)
     in if T.length t' > n then T.take (n - 1) t' <> "…" else t'

readerBody :: St -> Post -> Widget Name
readerBody s p =
  vBox
    [ C.hCenter . hLimit columnWidth . padTop (Pad 1) . padLeftRight 2 $
        vBox
          [ case burstAge s BTitle of
              Just (age, salt) -> scramble (9 + salt * 17) age headingAttr (postTitle p)
              Nothing -> withAttr headingAttr (txt (postTitle p))
          , withAttr metaAttr (txt (postDate p <> tagSuffix p))
          ]
    , viewport ReaderVP Vertical $
        C.hCenter (hLimit columnWidth (padTopBottom 1 (padLeftRight 2 (renderMarkdown linkify (postBody p)))))
    ]

tagSuffix :: Post -> Text
tagSuffix p
  | null (postTags p) = ""
  | otherwise = " · " <> T.intercalate ", " (postTags p)

-- Events ---------------------------------------------------------------

handle :: BrickEvent Name Tick -> EventM Name St ()
handle ev = do
  s <- get
  case ev of
    AppEvent Tick -> tick s
    _ -> modify (\st -> st {stLastInput = stTick st})
  case ev of
    AppEvent Tick -> pure ()
    MouseDown n V.BLeft _ (Location (c, r)) -> clickOn n c r
    MouseDown _ V.BScrollDown _ _ -> bump 0.2 >> wheel (stView s) 3
    MouseDown _ V.BScrollUp _ _ -> bump 0.2 >> wheel (stView s) (-3)
    -- brick only turns a click into a MouseDown event when the TOPMOST
    -- extent at that spot is clickable; anything under a viewport gets
    -- shadowed and arrives as a raw vty event instead. Hit-test the
    -- extents ourselves so links and post rows work.
    VtyEvent (V.EvMouseDown c r V.BLeft _) -> do
      exts <- findClickedExtents (c, r)
      case [e | e@(Extent nm _ _) <- exts, actionable nm] of
        Extent nm (Location (ec, er)) _ : _ -> clickOn nm (c - ec) (r - er)
        [] -> pure ()
    VtyEvent (V.EvMouseDown _ _ V.BScrollDown _) -> bump 0.2 >> wheel (stView s) 3
    VtyEvent (V.EvMouseDown _ _ V.BScrollUp _) -> bump 0.2 >> wheel (stView s) (-3)
    VtyEvent vev -> do
      modify (\st -> st {stStatus = Nothing})
      bump 0.25
      case hintIdxFor (stView s) vev of
        Just i -> setBurst (BHint i)
        Nothing -> pure ()
      key s vev
    _ -> pure ()

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
  LogoField ->
    modify (\st -> st {stRipple = Just (lc, lr, stTick st), stEnergy = min 1 (stEnergy st + 0.5)})
  LinkTo u -> bump 0.45 >> linkAction u
  PostRow i -> bump 0.45 >> zoom listL (modify (listMoveTo i)) >> openSelected
  TabBtn t -> bump 0.45 >> switchTab t
  BrandBtn -> bump 0.45 >> toLanding
  _ -> pure ()

-- Interaction feeds the glitch; idleness starves it.
bump :: Double -> EventM Name St ()
bump d = modify (\s -> s {stEnergy = min 1 (stEnergy s + d)})

-- Resource guards: sessions end after 10 idle minutes or 2 hours flat,
-- so parked or hostile connections can't hold memory forever.
idleTimeoutTicks, sessionCapTicks :: Int
idleTimeoutTicks = 10 * 60 * 10
sessionCapTicks = 2 * 60 * 60 * 10

tick :: St -> EventM Name St ()
tick s = do
  when
    (stTick s - stLastInput s > idleTimeoutTicks || stTick s > sessionCapTicks)
    halt
  modify $ \st ->
    st
      { stTick = stTick s + 1
      , stEnergy = stEnergy s * 0.88
      , stRipple = case stRipple s of
          Just (_, _, t0) | stTick s - t0 > 14 -> Nothing
          r -> r
      , stBurst = case stBurst s of
          Just (_, t0) | stTick s - t0 > burstFrames -> Nothing
          b -> b
      }

burstFrames :: Int
burstFrames = 5

-- Age of an active burst plus a per-burst salt so no two bursts
-- scramble identically.
burstAge :: St -> BurstTarget -> Maybe (Int, Int)
burstAge s bt = case stBurst s of
  Just (t, t0) | t == bt, stTick s - t0 <= burstFrames -> Just (stTick s - t0, t0)
  _ -> Nothing

setBurst :: BurstTarget -> EventM Name St ()
setBurst bt = modify (\s -> s {stBurst = Just (bt, stTick s)})

-- Scramble-in: text resolves out of hot static over burstFrames ticks.
scramble :: Int -> Int -> AttrName -> Text -> Widget Name
scramble seed age baseAttr t =
  hBox
    [ withAttr a (txt (T.singleton c'))
    | (i, c) <- zip [0 ..] (T.unpack t)
    , let h = (i * 131 + seed * 7919 + age * 3571) `mod` 101
          thr = [88, 55, 30, 12, 4] !! min age 4
          hit = h < thr && c /= ' '
          c' = if hit then "%#*=:@" !! (h `mod` 6) else c
          a = if hit then logoHotAttr else baseAttr
    ]

key :: St -> V.Event -> EventM Name St ()
key s vev = case stView s of
  Landing -> case vev of
    V.EvKey (V.KChar 'q') [] -> halt
    V.EvKey V.KEsc [] -> halt
    V.EvKey V.KEnter [] -> switchTab (stSel s)
    V.EvKey (V.KChar '\t') [] -> select nextTab
    V.EvKey (V.KChar 'l') [] -> select nextTab
    V.EvKey V.KRight [] -> select nextTab
    V.EvKey (V.KChar 'h') [] -> select prevTab
    V.EvKey V.KLeft [] -> select prevTab
    V.EvKey (V.KChar c) [] | Just t <- numTab c -> switchTab t
    _ -> pure ()
  PageView ThoughtsTab -> case vev of
    V.EvKey V.KEnter [] -> openSelected
    V.EvKey (V.KChar 'q') [] -> halt
    V.EvKey V.KEsc [] -> toLanding
    V.EvKey (V.KChar '\t') [] -> switchTab (nextTab ThoughtsTab)
    V.EvKey (V.KChar c) [] | Just t <- numTab c -> switchTab t
    _ | Just t <- tabMove vev ThoughtsTab -> switchTab t
    _ -> listNav (zoom listL (handleListEventVi handleListEvent vev))
  PageView t -> case vev of
    V.EvKey (V.KChar 'q') [] -> halt
    V.EvKey V.KEsc [] -> toLanding
    V.EvKey (V.KChar '\t') [] -> switchTab (nextTab t)
    V.EvKey (V.KChar c) [] | Just t' <- numTab c -> switchTab t'
    _ | Just t' <- tabMove vev t -> switchTab t'
    _ -> scrollKeys (viewportScroll (PageVP t)) vev
  PostView _ -> case vev of
    V.EvKey (V.KChar 'q') [] -> backToList
    V.EvKey V.KEsc [] -> backToList
    V.EvKey (V.KChar 'h') [] -> backToList
    V.EvKey V.KLeft [] -> backToList
    V.EvKey (V.KChar c) [] | Just t <- numTab c -> switchTab t
    _ -> scrollKeys (viewportScroll ReaderVP) vev

-- h/l and left/right switch pages, vim-style.
tabMove :: V.Event -> Tab -> Maybe Tab
tabMove (V.EvKey (V.KChar 'h') []) t = Just (prevTab t)
tabMove (V.EvKey V.KLeft []) t = Just (prevTab t)
tabMove (V.EvKey (V.KChar 'l') []) t = Just (nextTab t)
tabMove (V.EvKey V.KRight []) t = Just (nextTab t)
tabMove _ _ = Nothing

-- Run a list movement and burst the newly selected title if it moved.
listNav :: EventM Name St () -> EventM Name St ()
listNav act = do
  before <- (listSelected . stList) <$> get
  act
  after <- (listSelected . stList) <$> get
  when (before /= after) (maybe (pure ()) (setBurst . BPost) after)

wheel :: View -> Int -> EventM Name St ()
wheel v d = case v of
  PageView ThoughtsTab -> listNav (zoom listL (modify (listMoveBy (signum d))))
  PageView t -> vScrollBy (viewportScroll (PageVP t)) d
  PostView _ -> vScrollBy (viewportScroll ReaderVP) d
  Landing -> pure ()

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

-- Internal links navigate in-app; everything else surfaces in the
-- status line (and stays OSC 8 clickable in capable terminals).
linkAction :: Text -> EventM Name St ()
linkAction u
  | Just slug <- T.stripPrefix (siteBase <> "/thoughts/") u = openSlug (T.takeWhile (/= '#') slug)
  | u == siteBase || u == siteBase <> "/" = switchTab HomeTab
  | u == siteBase <> "/thoughts" = switchTab ThoughtsTab
  | u == siteBase <> "/etc" = switchTab EtcTab
  | otherwise = modify (\s -> s {stStatus = Just u, stBurst = Just (BStatus, stTick s)})

openSlug :: Text -> EventM Name St ()
openSlug slug = case find ((== slug) . postSlug) allPosts of
  Just p -> openPost p
  Nothing -> modify (\s -> s {stStatus = Just (siteBase <> "/thoughts/" <> slug)})

select :: (Tab -> Tab) -> EventM Name St ()
select f =
  modify (\s -> let t = f (stSel s) in s {stSel = t, stBurst = Just (BTab t, stTick s)})

switchTab :: Tab -> EventM Name St ()
switchTab t =
  modify (\s -> s {stView = PageView t, stSel = t, stBurst = Just (BTab t, stTick s)})

-- Landing selection resumes on the tab you were just on; the logo
-- re-materializes out of full static.
toLanding :: EventM Name St ()
toLanding =
  modify (\s -> s {stView = Landing, stEnergy = 1, stSel = maybe (stSel s) id (currentTab (stView s))})

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
