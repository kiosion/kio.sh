module Main (main) where

import qualified Data.Text.IO as TIO
import System.IO (hIsTerminalDevice, stdin)

import Content (allPosts, plainListing)
import UI (runTui)

main :: IO ()
main = do
  interactive <- hIsTerminalDevice stdin
  if interactive
    then runTui
    else TIO.putStr (plainListing allPosts)
