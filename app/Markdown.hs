{-# LANGUAGE OverloadedStrings #-}

-- Markdown -> brick widgets. Wrapping is width-aware via the render
-- context, so resizing the terminal reflows text for free. Footnote
-- refs ([^label]) become superscripts with a numbered section at the
-- bottom. Links are OSC 8 hyperlinks (modifier-click in capable
-- terminals) AND wrapped by the caller-supplied `mkLink` so the UI can
-- make them clickable in-app.
module Markdown (renderMarkdown, markdownAttrs, headingAttr, siteBase) where

import Brick
import qualified Brick.Widgets.Border as B
import CMark
import Data.Char (digitToInt)
import Data.List (intersperse, nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro ((^.))

siteBase :: Text
siteBase = "https://kio.dev"

plainAttr, headingAttr, codeAttr, emphAttr, strongAttr, linkAttr, quoteAttr, dimAttr :: AttrName
plainAttr = attrName "md-plain"
headingAttr = attrName "md-heading"
codeAttr = attrName "md-code"
emphAttr = attrName "md-emph"
strongAttr = attrName "md-strong"
linkAttr = attrName "md-link"
quoteAttr = attrName "md-quote"
dimAttr = attrName "md-dim"

markdownAttrs :: [(AttrName, V.Attr)]
markdownAttrs =
  [ (plainAttr, V.defAttr)
  , (headingAttr, V.defAttr `V.withStyle` V.bold `V.withForeColor` V.cyan)
  , (codeAttr, V.defAttr `V.withForeColor` V.yellow)
  , (emphAttr, V.defAttr `V.withStyle` V.italic)
  , (strongAttr, V.defAttr `V.withStyle` V.bold)
  , (linkAttr, V.defAttr `V.withStyle` V.underline `V.withForeColor` V.blue)
  , (quoteAttr, V.defAttr `V.withForeColor` V.green)
  , (dimAttr, V.defAttr `V.withStyle` V.dim)
  ]

type MkLink n = Text -> Widget n -> Widget n

renderMarkdown :: Ord n => MkLink n -> Text -> Widget n
renderMarkdown mk src =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx ^. availWidthL
        (body, notes) = extractFootnotes src
    render . vBox $
      blocks mk w (commonmarkToNode [] body)
        : (if null notes then [] else blank : footnoteWidgets mk w notes)

blank :: Widget n
blank = txt " "

-- Footnotes ------------------------------------------------------------

sup :: Int -> Text
sup = T.map digit . T.pack . show
 where
  digit c = "⁰¹²³⁴⁵⁶⁷⁸⁹" `T.index` digitToInt c

isFence :: Text -> Bool
isFence = T.isPrefixOf "```" . T.stripStart

-- Pull [^label]: definitions out of the body and replace [^label] refs
-- with superscript numbers (assigned in order of first reference).
-- Lines inside code fences are left untouched.
extractFootnotes :: Text -> (Text, [(Int, Text)])
extractFootnotes src = (T.unlines (replaceRefs table bodyLs), numbered <> extra)
 where
  (bodyLs, defs) = splitDefs (T.lines src)
  table = zip (nub (concat (outsideFences refsInLine bodyLs))) [1 ..]
  numbered = sortOn fst [(n, t) | (lbl, t) <- defs, Just n <- [lookup lbl table]]
  extra = zip [length table + 1 ..] [t | (lbl, t) <- defs, lookup lbl table == Nothing]

splitDefs :: [Text] -> ([Text], [(Text, Text)])
splitDefs = go False
 where
  go _ [] = ([], [])
  go fence (l : rest)
    | isFence l = keep l (go (not fence) rest)
    | not fence, Just (lbl, txt0) <- defStart l =
        let (cont, rest') = span isCont rest
            (b, ds) = go fence rest'
         in (b, (lbl, T.strip (T.unwords (txt0 : map T.strip cont))) : ds)
    | otherwise = keep l (go fence rest)
  keep l (b, ds) = (l : b, ds)
  isCont x = not (T.null (T.strip x)) && defStart x == Nothing && not (isFence x)

defStart :: Text -> Maybe (Text, Text)
defStart l = do
  r <- T.stripPrefix "[^" l
  let (lbl, rest) = T.breakOn "]:" r
  body <- T.stripPrefix "]:" rest
  if T.null lbl || T.any (== ']') lbl then Nothing else Just (lbl, T.strip body)

outsideFences :: (Text -> a) -> [Text] -> [a]
outsideFences f = go False
 where
  go _ [] = []
  go fence (l : rest)
    | isFence l = go (not fence) rest
    | fence = go fence rest
    | otherwise = f l : go fence rest

refsInLine :: Text -> [Text]
refsInLine t = case T.breakOn "[^" t of
  (_, "") -> []
  (_, rest) ->
    let r = T.drop 2 rest
        (lbl, after) = T.breakOn "]" r
     in if T.null after
          then []
          else
            let after' = T.drop 1 after
             in (if T.isPrefixOf ":" after' || T.null lbl then [] else [lbl])
                  <> refsInLine after'

replaceRefs :: [(Text, Int)] -> [Text] -> [Text]
replaceRefs table = go False
 where
  go _ [] = []
  go fence (l : rest)
    | isFence l = l : go (not fence) rest
    | fence = l : go fence rest
    | otherwise = repl l : go fence rest
  repl t = case T.breakOn "[^" t of
    (pre, "") -> pre
    (pre, rest) ->
      let r = T.drop 2 rest
          (lbl, after) = T.breakOn "]" r
       in if T.null after
            then t
            else
              let after' = T.drop 1 after
               in case lookup lbl table of
                    Just n
                      | not (T.isPrefixOf ":" after') ->
                          pre <> sup n <> repl after'
                    _ -> pre <> "[^" <> lbl <> "]" <> repl after'

footnoteWidgets :: Ord n => MkLink n -> Int -> [(Int, Text)] -> [Widget n]
footnoteWidgets mk w notes =
  withAttr dimAttr B.hBorder
    : [ hBox
          [ withAttr dimAttr (txt (sup n <> " "))
          , hLimit (max 8 (w - 3)) (blocks mk (w - 3) (commonmarkToNode [] t))
          ]
      | (n, t) <- notes
      ]

-- Blocks ---------------------------------------------------------------

blocks :: Ord n => MkLink n -> Int -> Node -> Widget n
blocks mk w (Node _ DOCUMENT ns) = vBox (intersperse blank (map (block mk w) ns))
blocks mk w n = block mk w n

block :: Ord n => MkLink n -> Int -> Node -> Widget n
block mk w (Node _ PARAGRAPH ns) = wrapFrags mk w (inlines ns)
block mk w (Node _ (HEADING lvl) ns) =
  wrapFrags mk w (Frag headingAttr Nothing (T.replicate lvl "#" <> " ") : map (reattr headingAttr) (inlines ns))
block _ _ (Node _ (CODE_BLOCK _ code) _) =
  padLeft (Pad 2) $
    withAttr codeAttr $
      vBox [txt (if T.null l then " " else l) | l <- T.lines code]
-- Render the body first so the border bar can match its full height.
block mk w (Node _ BLOCK_QUOTE ns) =
  Widget Fixed Fixed $ do
    inner <- render (hLimit (max 8 (w - 2)) (vBox (map (block mk (w - 2)) ns)))
    let ht = V.imageHeight (inner ^. imageL)
    render $
      hBox
        [ withAttr quoteAttr (vBox (replicate (max 1 ht) (txt "│ ")))
        , Widget Fixed Fixed (pure inner)
        ]
block mk w (Node _ (LIST attrs) items) =
  vBox (zipWith (listItem mk w attrs) [listStart attrs ..] items)
block _ _ (Node _ THEMATIC_BREAK _) = B.hBorder
block _ _ (Node _ (HTML_BLOCK t) _) = withAttr dimAttr (txt (T.strip t))
block mk w (Node _ _ ns) = vBox (map (block mk w) ns)

listItem :: Ord n => MkLink n -> Int -> ListAttributes -> Int -> Node -> Widget n
listItem mk w attrs i (Node _ ITEM ns) =
  hBox
    [ withAttr dimAttr (txt bullet)
    , hLimit (max 8 (w - T.length bullet)) (vBox (map (block mk (w - T.length bullet)) ns))
    ]
 where
  bullet = case listType attrs of
    BULLET_LIST -> "• "
    ORDERED_LIST -> T.pack (show i) <> ". "
listItem mk w _ _ n = block mk w n

-- Inlines --------------------------------------------------------------

data Frag = Frag AttrName (Maybe Text) Text

reattr :: AttrName -> Frag -> Frag
reattr a (Frag p u t) = Frag (if p == plainAttr then a else p) u t

-- Internal paths become absolute site URLs; in-page anchors have no
-- terminal equivalent.
resolveUrl :: Text -> Maybe Text
resolveUrl u
  | T.isPrefixOf "#" u = Nothing
  | T.isPrefixOf "/" u = Just (siteBase <> u)
  | otherwise = Just u

-- Dim host hint so links stay legible in terminals without OSC 8.
domainSuffix :: Text -> [Frag]
domainSuffix u
  | T.isPrefixOf "http" u =
      let host = T.takeWhile (/= '/') (T.drop 2 (snd (T.breakOn "//" u)))
       in [Frag dimAttr Nothing ("(" <> host <> ")") | not (T.null host)]
  | otherwise = []

inlines :: [Node] -> [Frag]
inlines = concatMap inline

inline :: Node -> [Frag]
inline (Node _ (TEXT t) _) = [Frag plainAttr Nothing t]
inline (Node _ SOFTBREAK _) = [Frag plainAttr Nothing " "]
inline (Node _ LINEBREAK _) = [Frag plainAttr Nothing " "]
inline (Node _ (CODE t) _) = [Frag codeAttr Nothing t]
inline (Node _ EMPH ns) = map (reattr emphAttr) (inlines ns)
inline (Node _ STRONG ns) = map (reattr strongAttr) (inlines ns)
inline (Node _ (LINK url _) ns) =
  map (withUrl (resolveUrl url) . reattr linkAttr) (inlines ns) <> domainSuffix url
inline (Node _ (IMAGE url _) ns) =
  Frag dimAttr Nothing "[image:"
    : map (withUrl (resolveUrl url) . reattr linkAttr) (inlines ns)
      <> (Frag dimAttr Nothing "]" : domainSuffix url)
inline (Node _ (HTML_INLINE t) _) = [Frag dimAttr Nothing t]
inline (Node _ _ ns) = inlines ns

withUrl :: Maybe Text -> Frag -> Frag
withUrl u (Frag a _ t) = Frag a u t

-- Greedy word wrap over attributed fragments. Splitting on whitespace
-- collapses runs of spaces inside inline code spans; fine for prose.
wrapFrags :: Ord n => MkLink n -> Int -> [Frag] -> Widget n
wrapFrags mk w frags = vBox (map line (greedyWrap (max 8 w) ws))
 where
  ws = [Frag a u word | Frag a u t <- frags, word <- T.words t]
  line [] = blank
  line fs = hBox (intersperse (txt " ") (map fragW fs))
  fragW (Frag a mu t) =
    case mu of
      Nothing -> withAttr a (txt t)
      Just u -> mk u (hyperlink u (withAttr a (txt t)))

greedyWrap :: Int -> [Frag] -> [[Frag]]
greedyWrap w = go [] 0
 where
  go acc _ [] = [reverse acc | not (null acc)]
  go acc len (f@(Frag _ _ t) : rest)
    | null acc = go [f] (T.length t) rest
    | len + 1 + T.length t <= w = go (f : acc) (len + 1 + T.length t) rest
    | otherwise = reverse acc : go [f] (T.length t) rest
