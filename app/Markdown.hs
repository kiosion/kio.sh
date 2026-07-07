{-# LANGUAGE OverloadedStrings #-}

-- Markdown to brick widgets
module Markdown
  ( RenderOpts (..),
    renderMarkdown,
    markdownAttrs,
    headingAttr,
    siteBase,
    greedyGroups,
  )
where

import Brick
import Brick.Widgets.Border qualified as B
import CMark
import Data.List (intersperse, nub, sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Lens.Micro ((^.))

siteBase :: Text
siteBase = "https://kio.dev"

data RenderOpts n = RenderOpts
  { roLink :: Text -> Widget n -> Widget n,
    roQuery :: Maybe Text, -- lowercased search query
    roHit :: Int, -- which match to scroll to
    roPing :: Bool -- apply the scroll request this render only
  }

plainAttr, headingAttr, codeAttr, emphAttr, strongAttr, linkAttr, quoteAttr, dimAttr, searchAttr :: AttrName
plainAttr = attrName "md-plain"
headingAttr = attrName "md-heading"
codeAttr = attrName "md-code"
emphAttr = attrName "md-emph"
strongAttr = attrName "md-strong"
linkAttr = attrName "md-link"
quoteAttr = attrName "md-quote"
dimAttr = attrName "md-dim"
searchAttr = attrName "md-search"

markdownAttrs :: [(AttrName, V.Attr)]
markdownAttrs =
  [ (plainAttr, V.defAttr),
    (headingAttr, V.defAttr `V.withStyle` V.bold `V.withForeColor` V.cyan),
    (codeAttr, V.defAttr `V.withForeColor` V.yellow),
    (emphAttr, V.defAttr `V.withStyle` V.italic),
    (strongAttr, V.defAttr `V.withStyle` V.bold),
    (linkAttr, V.defAttr `V.withStyle` V.underline `V.withForeColor` V.blue),
    (quoteAttr, V.defAttr `V.withForeColor` V.green),
    (dimAttr, V.defAttr `V.withStyle` V.dim),
    (searchAttr, V.black `on` V.yellow)
  ]

tshow :: Int -> Text
tshow = T.pack . show

renderMarkdown :: (Ord n) => RenderOpts n -> Text -> Widget n
renderMarkdown opts src =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx ^. availWidthL
        (body, notes) = extractFootnotes src
        topBlocks = case commonmarkToNode [] body of
          Node _ DOCUMENT ns -> ns
          n -> [n]
        active = activeBlock opts topBlocks
        blockW i n = (if Just i == active then visible else id) (block opts w n)
        bodyW = vBox (intersperse blank (zipWith blockW [0 ..] topBlocks))
    render . vBox $
      bodyW : (if null notes then [] else blank : footnoteWidgets opts w notes)

blank :: Widget n
blank = txt " "

matchesQuery :: RenderOpts n -> Text -> Bool
matchesQuery opts t = case roQuery opts of
  Just q -> not (T.null q) && T.isInfixOf q (T.toLower t)
  Nothing -> False

-- Which top-level block holds the roHit-th search match.
activeBlock :: RenderOpts n -> [Node] -> Maybe Int
activeBlock opts ns = case roQuery opts of
  Just q
    | roPing opts,
      not (T.null q) ->
        let counts = map (T.count q . T.toLower . nodeText) ns
            total = sum counts
         in if total == 0
              then Nothing
              else go (roHit opts `mod` total) 0 0 counts
  _ -> Nothing
  where
    go _ _ _ [] = Nothing
    go k i acc (c : cs)
      | acc + c > k = Just i
      | otherwise = go k (i + 1) (acc + c) cs

nodeText :: Node -> Text
nodeText (Node _ ty ns) = own <> T.concat (map nodeText ns)
  where
    own = case ty of
      TEXT t -> t
      CODE t -> t
      CODE_BLOCK _ t -> t
      HTML_BLOCK t -> t
      HTML_INLINE t -> t
      _ -> ""

-- Sentinels wrapping a ref's number so it survives the CommonMark
-- parse and can be turned into a clickable sup after.
fnA, fnB :: Char
fnA = '\xFFF9'
fnB = '\xFFFA'

isFence :: Text -> Bool
isFence = T.isPrefixOf "```" . T.stripStart

-- Pull [^label]: definitions out of the body and replace [^label] refs
-- with sentinel-wrapped numbers (assigned in order of first reference).
-- Leave lines inside code fences. Sentinels are stripped from the
-- source first so every one downstream is ours.
extractFootnotes :: Text -> (Text, [(Int, Text)])
extractFootnotes src = (T.unlines (replaceRefs table bodyLs), numbered <> extra)
  where
    (bodyLs, defs) = splitDefs (T.lines (T.filter (\c -> c /= fnA && c /= fnB) src))
    table = zip (nub (concat (outsideFences refsInLine bodyLs))) [1 ..]
    numbered = sortOn fst [(n, t) | (lbl, t) <- defs, Just n <- [lookup lbl table]]
    extra = zip [length table + 1 ..] [t | (lbl, t) <- defs, lookup lbl table == Nothing]

splitDefs :: [Text] -> ([Text], [(Text, Text)])
splitDefs = go False
  where
    go _ [] = ([], [])
    go fence (l : rest)
      | isFence l = keep l (go (not fence) rest)
      | not fence,
        Just (lbl, txt0) <- defStart l =
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
                            pre <> T.singleton fnA <> tshow n <> T.singleton fnB <> repl after'
                      _ -> pre <> "[^" <> lbl <> "]" <> repl after'

footnoteWidgets :: (Ord n) => RenderOpts n -> Int -> [(Int, Text)] -> [Widget n]
footnoteWidgets opts w notes =
  withAttr dimAttr B.hBorder
    : withAttr headingAttr (txt "Footnotes:")
    : [ hBox
          [ withAttr plainAttr (txt (tshow n <> ".")),
            txt " ",
            indented w 4 (\w' -> blocks opts w' (commonmarkToNode [] t))
          ]
      | (n, t) <- notes
      ]

blocks :: (Ord n) => RenderOpts n -> Int -> Node -> Widget n
blocks opts w (Node _ DOCUMENT ns) = vBox (intersperse blank (map (block opts w) ns))
blocks opts w n = block opts w n

-- render nested content k cells narrower, capped to that width
indented :: Int -> Int -> (Int -> Widget n) -> Widget n
indented w k f = hLimit (max 8 (w - k)) (f (w - k))

block :: (Ord n) => RenderOpts n -> Int -> Node -> Widget n
block opts w (Node _ PARAGRAPH ns) = wrapFrags opts w (inlines ns)
block opts w (Node _ (HEADING lvl) ns) =
  wrapFrags opts w (Frag headingAttr Nothing (T.replicate lvl "#" <> " ") : map (reattr headingAttr) (inlines ns))
block opts _ (Node _ (CODE_BLOCK _ code) _) =
  padLeft (Pad 2) $
    vBox
      [ withAttr (if matchesQuery opts l then searchAttr else codeAttr) (txt (if T.null l then " " else l))
      | l <- T.lines code
      ]
-- Render the body first so the border bar can match its full height.
block opts w (Node _ BLOCK_QUOTE ns) =
  Widget Fixed Fixed $ do
    inner <- render (indented w 2 (\w' -> vBox (map (block opts w') ns)))
    let ht = V.imageHeight (inner ^. imageL)
    render $
      hBox
        [ withAttr quoteAttr (vBox (replicate (max 1 ht) (txt "│ "))),
          Widget Fixed Fixed (pure inner)
        ]
block opts w (Node _ (LIST attrs) items) =
  vBox (zipWith (listItem opts w attrs) [listStart attrs ..] items)
block _ _ (Node _ THEMATIC_BREAK _) = B.hBorder
block _ _ (Node _ (HTML_BLOCK t) _) = withAttr dimAttr (txt (T.strip t))
block opts w (Node _ _ ns) = vBox (map (block opts w) ns)

listItem :: (Ord n) => RenderOpts n -> Int -> ListAttributes -> Int -> Node -> Widget n
listItem opts w attrs i (Node _ ITEM ns) =
  hBox
    [ withAttr dimAttr (txt bullet),
      indented w (T.length bullet) (\w' -> vBox (map (block opts w') ns))
    ]
  where
    bullet = case listType attrs of
      BULLET_LIST -> "• "
      ORDERED_LIST -> T.pack (show i) <> ". "
listItem opts w _ _ n = block opts w n

data Frag = Frag AttrName (Maybe Text) Text

reattr :: AttrName -> Frag -> Frag
reattr a (Frag p u t) = Frag (if p == plainAttr then a else p) u t

-- Internal paths become absolute site URLs.
resolveUrl :: Text -> Maybe Text
resolveUrl u
  | T.isPrefixOf "#" u = Nothing
  | T.isPrefixOf "/" u = Just (siteBase <> u)
  | otherwise = Just u

-- Dim host hint so links stay legible in terms w/out OSC 8.
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

-- A word containing a footnote sentinel becomes a link-coloured "(n)".
fnWord :: Frag -> Frag
fnWord f@(Frag _ _ w) = case T.breakOn (T.singleton fnA) w of
  (_, "") -> f
  (pre, rest) ->
    let (numT, rest2) = T.breakOn (T.singleton fnB) (T.drop 1 rest)
        post = T.filter (\c -> c /= fnA && c /= fnB) (T.drop 1 rest2)
     in Frag linkAttr Nothing (pre <> "(" <> numT <> ")" <> post)

-- Greedy wordwrap over attributed fragments. Splitting on whitespace
-- collapses runs of spaces inside inline code spans; fine for prose
wrapFrags :: (Ord n) => RenderOpts n -> Int -> [Frag] -> Widget n
wrapFrags opts w frags = vBox (map line (greedyWrap (max 8 w) ws))
  where
    ws = map fnWord [Frag a u word | Frag a u t <- frags, word <- T.words t]
    line [] = blank
    line fs = hBox (intersperse (txt " ") (map fragW fs))
    fragW (Frag a mu t) =
      let a' = if matchesQuery opts t then searchAttr else a
          w' = withAttr a' (txt t)
       in case mu of
            Nothing -> w'
            Just u -> roLink opts u (hyperlink u w')

greedyWrap :: Int -> [Frag] -> [[Frag]]
greedyWrap = greedyGroups (\(Frag _ _ t) -> T.length t) 1

-- Greedy line-fill: split items into rows whose total width (each item's
-- width plus `sep` between them) stays within `limit`. Shared by the
-- markdown, hint-bar, and plain-text wrappers.
greedyGroups :: (a -> Int) -> Int -> Int -> [a] -> [[a]]
greedyGroups width sep limit = go [] 0
  where
    go acc _ [] = [reverse acc | not (null acc)]
    go acc len (x : xs)
      | null acc = go [x] (width x) xs
      | len + sep + width x <= limit = go (x : acc) (len + sep + width x) xs
      | otherwise = reverse acc : go [x] (width x) xs
