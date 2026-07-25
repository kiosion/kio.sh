{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick.AttrMap (attrMap)
import Brick.Main (renderWidget)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp (..))
import Markdown (RenderOpts (..), greedyGroups, markdownAttrs, renderMarkdown)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative (..), Positive (..))

opts :: RenderOpts ()
opts =
  RenderOpts
    { roLink = \_ w -> w, -- Draw wraps links in a clickable extent; the text is the same
      roQuery = Nothing,
      roHit = 0,
      roPing = False
    }

screen :: Text -> [Text]
screen src = filter (not . T.null) (map row (V.toList (displayOpsForPic pic (w, h))))
  where
    (w, h) = (80, 40)
    widget = renderMarkdown opts src
    pic = renderWidget (Just (attrMap Vty.defAttr markdownAttrs)) [widget] (w, h)
    row r = T.stripEnd (T.concat [TL.toStrict (textSpanText o) | o@TextSpan {} <- V.toList r])

main :: IO ()
main = hspec $ do
  describe "renderMarkdown: raw HTML" $ do
    it "drops the <script> block of component imports" $
      screen "<script>\n  import Camel from '$components/figures/camel.svelte';\n</script>\n\nProse.\n"
        `shouldBe` ["Prose."]

    it "drops a component tag, keeping the prose around it" $
      screen "Before.\n\n<Camel />\n\nAfter.\n"
        `shouldBe` ["Before.", "After."]

    it "drops a multi-line component tag" $
      screen "Before.\n\n<Image\n  src=\"/a.png\"\n  alt=\"A screenshot\"\n/>\n\nAfter.\n"
        `shouldBe` ["Before.", "After."]

    it "keeps HTML written as a code span" $
      screen "Use `<b>...</b>` for bold.\n"
        `shouldBe` ["Use <b>...</b> for bold."]

  describe "renderMarkdown: prose" $ do
    it "renders a heading with its level marker" $
      screen "## Syntax\n"
        `shouldBe` ["## Syntax"]

    it "hints the host after a link" $
      screen "See [the docs](https://example.com/a).\n"
        `shouldSatisfy` any (T.isInfixOf "(example.com)")

    -- notes are separated from the body by a full-width rule
    it "numbers footnote references and lists the notes" $
      screen "Claim.[^a]\n\n[^a]: The note.\n"
        `shouldBe` ["Claim.(1)", T.replicate 80 "─", "Footnotes:", "1. The note."]

  -- Shared by markdown wrapping (greedyWrap), the title effect (Fx.wrapLines)
  -- and the hint bar (Draw)
  describe "greedyGroups" $ do
    let rows sep limit ws = greedyGroups T.length sep limit (map T.pack ws)
        rowWidth sep g = sum (map T.length g) + sep * max 0 (length g - 1)

    prop "keeps every item, in order" $ \(NonNegative sep) (Positive limit) ws ->
      concat (rows sep limit ws) == map T.pack ws

    prop "never emits an empty row" $ \(NonNegative sep) (Positive limit) ws ->
      all (not . null) (rows sep limit ws)

    prop "exceeds the limit only for an item that can't fit a row alone" $ \(NonNegative sep) (Positive limit) ws ->
      all (\g -> rowWidth sep g <= limit || length g == 1) (rows sep limit ws)
