{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Content
  ( Post (..),
    PageContent (..),
    allPosts,
    aboutPage,
    etcPage,
    contentValidation,
    plainListing,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, embedFile)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Yaml qualified as Yaml

-- mirror PostMetadata (src/lib/content.ts)
data Meta = Meta
  { mTitle :: Text,
    mDate :: Text,
    mDesc :: Maybe Text,
    mTags :: [Text],
    mDraft :: Bool
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
  { postSlug :: Text,
    postTitle :: Text,
    postDate :: Text,
    postDesc :: Maybe Text,
    postTags :: [Text],
    postBody :: Text
  }

postFiles :: [(FilePath, ByteString)]
postFiles = $(embedDir "content/posts")

allPosts :: [Post]
allPosts =
  sortOn
    (Down . postDate) -- ISO; lexical sort is chronological
    [p | (path, raw) <- postFiles, ".md" `isSuffixOf` path, Just p <- [toPost path raw]]

-- the loud error is deliberate here; <|> would read worse
{- HLINT ignore toPost "Use <|>" -}
toPost :: FilePath -> ByteString -> Maybe Post
toPost path raw = do
  -- Loud failure over a silently missing post; the Dockerfile smoke-run
  -- surfaces this at image build time.
  (meta, body) <-
    maybe
      (error ("kio-tui: bad/missing frontmatter in posts/" <> path))
      Just
      (parseFrontmatter (TE.decodeUtf8Lenient raw))
  if mDraft meta
    then Nothing
    else
      Just
        Post
          { postSlug = fromMaybe (T.pack path) (T.stripSuffix ".md" (T.pack path)),
            postTitle = mTitle meta,
            postDate = mDate meta,
            postDesc = mDesc meta,
            postTags = mTags meta,
            postBody = body
          }

data PageContent = PageContent
  { pcTitle :: [Text],
    pcBody :: Text
  }

-- page frontmatter title: a list of lines or a single string
newtype PageTitle = PageTitle [Text]

instance FromJSON PageTitle where
  parseJSON = withObject "page frontmatter" $ \o -> do
    v <- o .: "title"
    PageTitle <$> (parseJSON v <|> ((: []) <$> parseJSON v))

aboutPage :: PageContent
aboutPage = page "about.md" $(embedFile "content/about.md")

etcPage :: PageContent
etcPage = page "etc.md" $(embedFile "content/etc.md")

page :: FilePath -> ByteString -> PageContent
page path raw = case parseFrontmatter (TE.decodeUtf8Lenient raw) of
  Just (PageTitle t, body) -> PageContent t body
  Nothing -> error ("kio-tui: bad/missing frontmatter in content/" <> path)

contentValidation :: ()
contentValidation = aboutPage `seq` etcPage `seq` foldr seq () allPosts

parseFrontmatter :: (FromJSON meta) => Text -> Maybe (meta, Text)
parseFrontmatter t = case T.lines t of
  ("---" : rest) ->
    let (fm, remainder) = break (== "---") rest
     in case Yaml.decodeEither' (TE.encodeUtf8 (T.unlines fm)) of
          Right meta -> Just (meta, T.unlines (drop 1 remainder))
          Left _ -> Nothing
  _ -> Nothing

-- fall back for connections w/out pty (e.g. `ssh host < /dev/null`)
plainListing :: [Post] -> Text
plainListing ps =
  T.unlines $
    "kio.dev · posts (connect with a terminal for interactive)"
      : ""
      : concatMap entry ps
  where
    entry p =
      (postDate p <> "  " <> postTitle p)
        : maybe [] (\d -> ["          " <> d]) (postDesc p)
