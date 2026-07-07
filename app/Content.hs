{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Content
  ( Post (..)
  , PageContent (..)
  , allPosts
  , aboutPage
  , etcPage
  , plainListing
  ) where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, embedFile)
import Data.List (isSuffixOf, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml

-- Mirrors PostMetadata in src/lib/content.ts
data Meta = Meta
  { mTitle :: Text
  , mDate :: Text
  , mDesc :: Maybe Text
  , mTags :: [Text]
  , mDraft :: Bool
  }

instance FromJSON Meta where
  parseJSON = withObject "post frontmatter" $ \o ->
    Meta
      <$> o .: "title"
      <*> o .: "date"
      <*> o .:? "desc"
      <*> o .:? "tags" .!= []
      <*> o .:? "draft" .!= False

data Post = Post
  { postSlug :: Text
  , postTitle :: Text
  , postDate :: Text
  , postDesc :: Maybe Text
  , postTags :: [Text]
  , postBody :: Text
  }

-- Content is baked into the binary at compile time; rebuilding the image
-- after editing content is the whole deployment story.
postFiles :: [(FilePath, ByteString)]
postFiles = $(embedDir "../src/content/posts")

allPosts :: [Post]
allPosts =
  sortOn (Down . postDate) -- ISO dates, so lexical sort is chronological
    [p | (path, raw) <- postFiles, ".md" `isSuffixOf` path, Just p <- [toPost path raw]]

toPost :: FilePath -> ByteString -> Maybe Post
toPost path raw = do
  (meta, body) <- parseFrontmatter (TE.decodeUtf8Lenient raw)
  if mDraft meta
    then Nothing
    else
      Just
        Post
          { postSlug = maybe (T.pack path) id (T.stripSuffix ".md" (T.pack path))
          , postTitle = mTitle meta
          , postDate = mDate meta
          , postDesc = mDesc meta
          , postTags = mTags meta
          , postBody = body
          }

-- The about/etc pages (AboutMetadata / EtcMetadata in src/lib/content.ts).
data PageContent = PageContent
  { pcTitle :: [Text]
  , pcBody :: Text
  }

newtype AboutMeta = AboutMeta [Text]

instance FromJSON AboutMeta where
  parseJSON = withObject "about frontmatter" $ \o -> AboutMeta <$> o .: "title"

newtype TitleMeta = TitleMeta Text

instance FromJSON TitleMeta where
  parseJSON = withObject "page frontmatter" $ \o -> TitleMeta <$> o .: "title"

aboutPage :: PageContent
aboutPage =
  case parseFrontmatter (TE.decodeUtf8Lenient $(embedFile "../src/content/about.md")) of
    Just (AboutMeta t, body) -> PageContent t body
    Nothing -> PageContent ["kio.dev"] ""

etcPage :: PageContent
etcPage =
  case parseFrontmatter (TE.decodeUtf8Lenient $(embedFile "../src/content/etc.md")) of
    Just (TitleMeta t, body) -> PageContent [t] body
    Nothing -> PageContent ["Et cetera"] ""

parseFrontmatter :: FromJSON meta => Text -> Maybe (meta, Text)
parseFrontmatter t = case T.lines t of
  ("---" : rest) ->
    let (fm, remainder) = break (== "---") rest
     in case Yaml.decodeEither' (TE.encodeUtf8 (T.unlines fm)) of
          Right meta -> Just (meta, T.unlines (drop 1 remainder))
          Left _ -> Nothing
  _ -> Nothing

-- Fallback for connections without a pty (e.g. `ssh host < /dev/null`).
plainListing :: [Post] -> Text
plainListing ps =
  T.unlines $
    "kio.dev · posts (connect with a terminal for the interactive version)"
      : ""
      : concatMap entry ps
 where
  entry p =
    (postDate p <> "  " <> postTitle p)
      : maybe [] (\d -> ["          " <> d]) (postDesc p)
