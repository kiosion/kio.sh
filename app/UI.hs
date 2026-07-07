{-# LANGUAGE OverloadedStrings #-}

-- Terminal setup, tick source, initial state.
-- See Core (types/theme), Fx (effects), Draw (rendering), Events (input).
module UI (runTui) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.List (list)
import Content (allPosts)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Core
import Data.Char (isAlphaNum)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vec
import Draw (draw)
import Events (handle)
import GHC.Clock (getMonotonicTime)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform qualified as V (mkVty)
import System.Environment (lookupEnv)

runTui :: IO ()
runTui = do
  user <- (>>= sanitizeUser) <$> lookupEnv "USER"
  chan <- newBChan 16
  t0 <- getMonotonicTime
  lastInputRef <- newIORef t0
  -- Tick drives fx while active (Events.handle stamps lastInputRef on
  -- every real event); wall-clock timeouts stay enforced even while fx
  -- ticks are paused. Timings live in Core.
  void . forkIO . forever $ do
    threadDelay tickMicros
    now <- getMonotonicTime
    lastIn <- readIORef lastInputRef
    if now - lastIn > idleTimeoutSecs || now - t0 > sessionCapSecs
      then writeBChan chan TimeUp
      else when (now - lastIn <= fxIdleSecs) (writeBChan chan Tick)
  let buildVty = do
        v <- V.mkVty V.defaultConfig
        let out = V.outputIface v
        V.setMode out V.Mouse True
        when (V.supportsMode out V.Hyperlink) (V.setMode out V.Hyperlink True)
        pure v
  vty <- buildVty
  void $ customMain vty buildVty (Just chan) (app lastInputRef) (initialSt user)

-- keep only benign chars and cap length; user-controlled
sanitizeUser :: String -> Maybe Text
sanitizeUser name
  | T.null t || t == "blog" = Nothing
  | otherwise = Just t
  where
    t = T.take 16 (T.filter (\c -> isAlphaNum c || c `elem` ("-_." :: String)) (T.pack name))

initialSt :: Maybe Text -> St
initialSt user =
  St
    { stList = list PostList (Vec.fromList allPosts) 4,
      stView = Landing,
      stUser = user,
      stTick = 0,
      stSel = HomeTab,
      stEnergy = 1,
      stRipple = Nothing,
      stBurst = Nothing,
      stStatus = Nothing,
      stPrompt = Nothing,
      stQuery = Nothing,
      stPing = False,
      stFnJump = Nothing,
      stHelp = False,
      stProgress = Nothing,
      stMouseHeld = False
    }

app :: IORef Double -> App St Tick Name
app lastInputRef =
  App
    { appDraw = draw,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handle lastInputRef,
      appStartEvent = pure (),
      appAttrMap = const theMap
    }
