{-# LANGUAGE OverloadedStrings #-}

-- rendering stuff: landing, header/footer chrome, pages, list, reader.
module Draw (draw) where

import Brick
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.List (renderListWithIndex)
import Content
import Control.Applicative ((<|>))
import Core
import Data.List (intersperse, nub, sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Fx
import Lens.Micro ((^.))
import Markdown

-- attr-text shorthands; Draw is wall-to-wall meta/title text
mtxt, ttxt :: Text -> Widget Name
mtxt = withAttr metaAttr . txt
ttxt = withAttr titleAttr . txt

mwrap :: Text -> Widget Name
mwrap = withAttr metaAttr . txtWrap

-- centered reading column for every page body
column :: Widget Name -> Widget Name
column = C.hCenter . hLimit columnWidth

-- bottom-bar row w/ any content on the left, hints on the right
barRow :: Widget Name -> Text -> Widget Name
barRow left hint = hBox [left, padLeft Max (mtxt hint)]

draw :: St -> [Widget Name]
draw s = (if stHelp s then (helpOverlay :) else id) base
  where
    base = case stView s of
      Landing -> [drawLanding s]
      v -> [vBox [topBar s v, body v, bottomBar s v]]
    body (PageView ThoughtsTab) =
      vBox
        [ column . padTop (Pad 1) . padLeftRight 2 $
            vBox
              [ withAttr headingAttr (txt "Thoughts"),
                mwrap thoughtsSub,
                txt " "
              ],
          column (renderListWithIndex (postRow s) True (stList s))
        ]
    body (PageView t) =
      pageViewport t (contentPage (rOpts s) (if t == HomeTab then aboutPage else etcPage))
    body (PostView p) = readerBody s p
    body Landing = emptyWidget

-- Markdown render options derived from the current state
rOpts :: St -> RenderOpts Name
rOpts s =
  RenderOpts
    { roLink = linkify,
      roJump = stFnJump s,
      roQuery = T.toLower <$> (stPrompt s <|> (fst <$> stQuery s)),
      roHit = maybe 0 snd (stQuery s),
      roPing = stPing s
    }

drawLanding :: St -> Widget Name
drawLanding s =
  vBox
    [ C.center . clickable LogoField . hLimit logoW $
        glitchWidget logoArt s (stRipple s),
      centeredWrap titleAttr heroHead,
      centeredWrap metaAttr (heroRest <> greeting),
      txt " ",
      C.hCenter (tabRow s (Just (stSel s))),
      txt " ",
      centeredWrap metaAttr "h/l select · enter open · q quit · ? keys",
      txt " "
    ]
  where
    greeting = maybe "" (" · hi, " <>) (stUser s)
    (heroHead, heroRest) = case pcTitle aboutPage of
      (hd : rest) -> (hd, T.unwords rest)
      [] -> ("kio.dev", "")

-- Centered line that wraps on narrow terminals with each wrapped line
-- individually centered (txtWrap just left-aligns inside its box)
centeredWrap :: AttrName -> Text -> Widget Name
centeredWrap a t =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = max 8 (min 64 (ctx ^. availWidthL))
    render . vBox $ [C.hCenter (withAttr a (txt l)) | l <- wrapLines w t]

helpOverlay :: Widget Name
helpOverlay =
  C.centerLayer . B.borderWithLabel (ttxt " keys ") . padLeftRight 2 . padTopBottom 1 $
    vBox [row k d | (k, d) <- entries]
  where
    row k d = hBox [hLimit 10 (padRight Max (ttxt k)), mtxt d]
    entries =
      [ ("j/k", "scroll / select"),
        ("h/l", "switch page / back"),
        ("tab", "next page"),
        ("enter", "open"),
        ("esc", "back / clear search"),
        ("space/b", "reader: page down / up"),
        ("g/G", "reader: top / bottom"),
        ("y", "reader: copy post link"),
        ("o", "reader: show post link"),
        ("/ n N", "search / next / prev"),
        ("q", "quit"),
        ("?", "this help")
      ]

tabRow :: St -> Maybe Tab -> Widget Name
tabRow s active =
  hBox . intersperse (mtxt "  ·  ") $
    [ clickable (TabBtn t) (burstText s (BTab t) (tabSeed t) attr label)
    | (t, label) <- tabs,
      let attr = if Just t == active then activeTabAttr else metaAttr
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
          [ clickable BrandBtn (hLimit 11 (glitchWidget miniLogoArt s Nothing)),
            txt "   ",
            vLimit (length miniLogoArt) . C.vCenter . vBox $
              [ brandLine s,
                txt " ",
                tabRow s (currentTab v)
              ]
          ],
      glitchRule (stTick s)
    ]

-- todo: taken from about.md's closing line; actually add a site meta desc.
siteDesc :: Text
siteDesc = " · yapping about programming, security, & whatever else"

-- brand + site desc + greeting
brandLine :: St -> Widget Name
brandLine s =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx ^. availWidthL
        oneLine = 7 + T.length siteDesc + 1 + T.length hi <= w
    render $
      if oneLine
        then hBox [brand, mtxt siteDesc, greet]
        else vBox [hBox [brand, greet], mwrap siteDesc]
  where
    hi = maybe " " ("hi, " <>) (stUser s)
    brand = clickable BrandBtn (ttxt "kio.dev")
    greet = padLeft Max (mtxt hi)

thoughtsSub :: Text
thoughtsSub =
  T.pack (show (length allPosts))
    <> " posts"
    <> (if null topTags then "" else " · mostly " <> T.intercalate ", " topTags)
  where
    allTags = concatMap postTags allPosts
    topTags =
      take 3 . map fst . sortOn (negate . snd) $
        [(t, length (filter (== t) allTags)) | t <- nub allTags]

bottomBar :: St -> View -> Widget Name
bottomBar s v = padLeftRight 2 $ case (stPrompt s, stStatus s) of
  (Just buf, _) ->
    barRow
      (hBox [ttxt ("/" <> buf), withAttr activeTabAttr (txt "▌")])
      "enter search · esc cancel"
  (_, Just u) ->
    let sw = burstText s BStatus 4 titleAttr u
     in barRow
          (hBox [withAttr activeTabAttr (txt "→ "), if T.isPrefixOf "http" u then hyperlink u sw else sw])
          "any key dismisses"
  _
    | Just (q, _) <- stQuery s ->
        barRow (mtxt ("/" <> q)) "n/N next/prev · esc clear"
  _ ->
    hBox
      [ maybe (txt " ") (\pc -> mtxt (T.pack (show pc) <> "%")) (stProgress s),
        hintBar s v
      ]

-- right-aligned key hints
hintBar :: St -> View -> Widget Name
hintBar s v =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let avail = max 12 (ctx ^. availWidthL)
        rows = chunk avail (zip [0 ..] [(lbl, d) | (_, lbl, d) <- hintMap v])
    render . vBox $ [padLeft Max (hBox (intersperse (mtxt " · ") (map seg r))) | r <- rows]
  where
    seg (i, (lbl, d)) =
      hBox
        [ -- plain brief highlight on press
          withAttr (maybe metaAttr (const titleAttr) (burstAge s (BHint i))) (txt lbl),
          mtxt (" " <> d)
        ]
    segW (_, (lbl, d)) = T.length lbl + 1 + T.length d
    chunk avail = greedyGroups segW 3 avail

linkify :: Text -> Widget Name -> Widget Name
linkify u = clickable (LinkTo u)

pageViewport :: Tab -> Widget Name -> Widget Name
pageViewport t w =
  viewport (PageVP t) Vertical (column (padTopBottom 1 (padLeftRight 2 w)))

contentPage :: RenderOpts Name -> PageContent -> Widget Name
contentPage opts pc =
  vBox (titleLines <> [txt " ", renderMarkdown opts (pcBody pc)])
  where
    titleLines = case pcTitle pc of
      (hd : rest) ->
        withAttr headingAttr (txt hd) : [mtxt l | l <- rest]
      [] -> []

postRow :: St -> Int -> Bool -> Post -> Widget Name
postRow s i sel p =
  clickable (PostRow i) . padLeftRight 2 $
    vBox
      [ hBox
          [ withAttr tAttr (txt (if sel then "❯ " else "  ")),
            burstWrap s (BPost i) (i * 91) tAttr (postTitle p)
          ],
        padLeft (Pad 4) (mwrap descLine),
        padLeft (Pad 4) (mwrap (postDate p <> tagSuffix p)),
        txt " "
      ]
  where
    tAttr = if sel then activeTabAttr else titleAttr
    descLine = maybe " " (truncate1 descMax) (postDesc p)
    truncate1 n t =
      let t' = T.unwords (T.words t)
       in if T.length t' > n then T.take (n - 1) t' <> "…" else t'

readerBody :: St -> Post -> Widget Name
readerBody s p =
  vBox
    [ column . padTop (Pad 1) . padLeftRight 2 $
        vBox
          [ burstWrap s BTitle 9 headingAttr (postTitle p),
            mwrap (postDate p <> tagSuffix p)
          ],
      viewport ReaderVP Vertical $
        column (padTopBottom 1 (padLeftRight 2 (renderMarkdown (rOpts s) (postBody p))))
    ]

tagSuffix :: Post -> Text
tagSuffix p
  | null (postTags p) = ""
  | otherwise = " · " <> T.intercalate ", " (postTags p)
