{-# LANGUAGE OverloadedStrings #-}

-- Shared: widget names, views, app state, theme, and some small pure helpers
module Core
  ( Tab (..),
    Name (..),
    View (..),
    Tick (..),
    St (..),
    BurstTarget (..),
    listL,
    columnWidth,
    burstFrames,
    tickMicros,
    wheelStep,
    energyDecay,
    bumpKey,
    bumpWheel,
    bumpClick,
    bumpLogo,
    rippleFrames,
    descMax,
    fxIdleSecs,
    idleTimeoutSecs,
    sessionCapSecs,
    tabs,
    currentTab,
    nextTab,
    prevTab,
    hintMap,
    burstAge,
    titleAttr,
    metaAttr,
    activeTabAttr,
    logoDenseAttr,
    logoMidAttr,
    logoHotAttr,
    theMap,
  )
where

import Brick
import Brick.Widgets.List (GenericList, listSelectedAttr)
import Content (Post)
import Data.Text (Text)
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Lens.Micro (Lens', lens)
import Markdown (FnJump, markdownAttrs)

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

-- Tick drives fx at 10fps while the visitor is active; TimeUp is the
-- tick thread ordering a disconnect (idle timeout / session cap).
data Tick = Tick | TimeUp

data St = St
  { stList :: GenericList Name Vec.Vector Post,
    stView :: View,
    stUser :: Maybe Text,
    stTick :: Int,
    stSel :: Tab,
    stEnergy :: Double, -- interaction-fed glitch intensity, decays per tick
    stRipple :: Maybe (Int, Int, Int),
    stBurst :: Maybe (BurstTarget, Int),
    stStatus :: Maybe Text,
    stPrompt :: Maybe Text, -- "/" search prompt buffer while typing
    stQuery :: Maybe (Text, Int), -- committed search query + active hit
    stPing :: Bool, -- apply search scroll request this render
    stFnJump :: Maybe FnJump, -- footnote scroll request
    stHelp :: Bool, -- "?" keymap overlay showing
    stProgress :: Maybe Int, -- reader scroll percentage
    stMouseHeld :: Bool -- left button down; swallow drag repeats
  }

-- transient scramble-in fx targets
data BurstTarget = BTab Tab | BTitle | BStatus | BHint Int | BPost Int
  deriving (Eq)

listL :: Lens' St (GenericList Name Vec.Vector Post)
listL = lens stList (\s l -> s {stList = l})

-- Tuning: the numbers that shape how the app feels, in one place.

-- cap content column so wide terminals still read like a page
columnWidth :: Int
columnWidth = 92

-- scramble-in length (ticks)
burstFrames :: Int
burstFrames = 5

-- fx tick period; 10fps
tickMicros :: Int
tickMicros = 100000

-- rows per wheel notch
wheelStep :: Int
wheelStep = 3

-- glitch energy: per-tick falloff and per-interaction feeds
energyDecay, bumpKey, bumpWheel, bumpClick, bumpLogo :: Double
energyDecay = 0.88
bumpKey = 0.25
bumpWheel = 0.2
bumpClick = 0.45
bumpLogo = 0.5

-- logo click ripple lifetime (ticks)
rippleFrames :: Int
rippleFrames = 14

-- post-list description truncation (chars)
descMax :: Int
descMax = 84

-- fx ticks pause after 30s without input (brick only redraws on
-- events, so a paused session costs ~zero host CPU); disconnect at
-- 15m idle / 2h flat
fxIdleSecs, idleTimeoutSecs, sessionCapSecs :: Double
fxIdleSecs = 30
idleTimeoutSecs = 15 * 60
sessionCapSecs = 2 * 60 * 60

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

-- source-of-truth for key hints: which keys belong to a segment, its
-- label, and desc. Draw renders labels; Events matches keys against it
-- to flash the segment on press. Uncommon keys (y/o, g/G, ...) live in
-- the "?" overlay only.
hintMap :: View -> [([V.Key], Text, Text)]
hintMap v = case v of
  PageView ThoughtsTab -> jk : ([V.KEnter], "enter", "read") : hl : common
  PostView _ ->
    [ jk,
      ([V.KChar ' ', V.KChar 'b', V.KPageUp, V.KPageDown], "space/b", "page"),
      ([V.KEsc, V.KChar 'h', V.KLeft], "esc", "back"),
      search,
      help
    ]
  _ -> jk : hl : common
  where
    jk = (jkKeys, "j/k", "scroll")
    hl = (hlKeys, "h/l", "pages")
    common = [([V.KEsc], "esc", "landing"), ([V.KChar 'q'], "q", "quit"), search, help]
    search = ([V.KChar '/'], "/", "search")
    help = ([V.KChar '?'], "?", "keys")

jkKeys, hlKeys :: [V.Key]
jkKeys = [V.KChar 'j', V.KChar 'k', V.KUp, V.KDown]
hlKeys = [V.KChar 'h', V.KChar 'l', V.KChar '\t', V.KLeft, V.KRight]

burstAge :: St -> BurstTarget -> Maybe (Int, Int)
burstAge s bt = case stBurst s of
  Just (t, t0) | t == bt, stTick s - t0 <= burstFrames -> Just (stTick s - t0, t0)
  _ -> Nothing

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
    [ (titleAttr, V.defAttr `V.withStyle` V.bold),
      (metaAttr, V.defAttr `V.withStyle` V.dim),
      (activeTabAttr, V.defAttr `V.withForeColor` V.cyan `V.withStyle` V.bold `V.withStyle` V.underline),
      (listSelectedAttr, V.defAttr `V.withStyle` V.bold),
      (logoDenseAttr, V.defAttr `V.withForeColor` V.red),
      (logoMidAttr, V.defAttr `V.withForeColor` V.magenta `V.withStyle` V.dim),
      (logoHotAttr, V.defAttr `V.withForeColor` V.brightRed `V.withStyle` V.bold)
    ]
      <> markdownAttrs
