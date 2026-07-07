{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Content (Post (..), allPosts, plainListing)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdin)
import UI (runTui)

main :: IO ()
main = do
  user <- fromMaybe "" <$> lookupEnv "USER"
  if user `elem` botNames
    then do
      TIO.putStrLn ("hey, \"" <> T.pack user <> "\" -- pick another name :)")
      exitFailure
    else run

-- common credential-stuffer usernames we can ignore (mostly just bot noise)
botNames :: [String]
botNames =
  [ "admin",
    "administrator",
    "user",
    "test",
    "guest",
    "ubuntu",
    "debian",
    "centos",
    "oracle",
    "postgres",
    "mysql",
    "git",
    "ftpuser",
    "www-data",
    "support"
  ]

run :: IO ()
run = do
  cmd <- lookupEnv "SSH_ORIGINAL_COMMAND"
  interactive <- hIsTerminalDevice stdin
  case words (fromMaybe "" cmd) of
    ["ls"] -> TIO.putStr (T.unlines (map postSlug allPosts))
    ["cat", slug] -> case find ((== T.pack slug) . postSlug) allPosts of
      Just p -> TIO.putStr ("# " <> postTitle p <> "\n\n" <> postBody p)
      Nothing -> die ("cat: " <> slug <> ": no such content (try 'ssh kio.sh ls')")
    _ | interactive -> runTui
    [] -> TIO.putStr (plainListing allPosts)
    _ -> die "commands: ls, cat <slug>"
  where
    die msg = hPutStrLn stderr ("kio.sh: " <> msg) >> exitFailure
