{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Visual fx: logo renderer, text scramble-in.
module Fx
  ( logoArt,
    logoW,
    miniLogoArt,
    glitchWidget,
    glitchRule,
    burstText,
    burstWrap,
    wrapLines,
  )
where

import Brick
import Core
import Data.FileEmbed (embedFile)
import Data.List (groupBy)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lens.Micro ((^.))
import Markdown (greedyGroups)

-- single source of truth for the logo: ssh/logo.txt (the Dockerfile
-- also injects it into the HTTP landing page)
logoArt :: [Text]
logoArt = T.lines (TE.decodeUtf8Lenient $(embedFile "logo.txt"))

logoW :: Int
logoW = maximum (0 : map T.length logoArt)

miniLogoArt :: [Text]
miniLogoArt =
  [ " *@%.",
    "  .*@@@%.",
    " .%@   @@.",
    ".@+    * .",
    " *@%.  .%*",
    "   %*%@@."
  ]

-- deterministic per-cell noise for all flicker in the app
noise :: Int -> Int -> Int -> Int -> Int
noise a b t m = (a * 7919 + b * 104729 + t * 31337) `mod` m

-- logo art with per-tick glitch flicker; clicking scrambles a ripple ring
glitchWidget :: [Text] -> St -> [(Int, Int, Int)] -> Widget Name
glitchWidget art s ripple =
  vBox
    [ hBox [withAttr a (txt t) | (a, t) <- row]
    | row <- glitchArt art (stTick s) ripple
    ]

-- render text that scrambles-in while its burst target is active
burstText :: St -> BurstTarget -> Int -> AttrName -> Text -> Widget Name
burstText s target seed a t = case burstAge s target of
  Just (age, salt) -> scramble (seed + salt * 17) age a t
  Nothing -> withAttr a (txt t)

-- burstText that word-wraps to the available width (for titles that
-- can exceed narrow terminals)
burstWrap :: St -> BurstTarget -> Int -> AttrName -> Text -> Widget Name
burstWrap s target seed a t =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let ls = wrapLines (max 8 (ctx ^. availWidthL)) t
    render . vBox $ case burstAge s target of
      Just (age, salt) -> [scramble (seed + salt * 17 + j) age a l | (j, l) <- zip [0 ..] ls]
      Nothing -> [withAttr a (txt l) | l <- ls]

-- greedy word-wrap; wide chars count as 1 (fine for prose)
wrapLines :: Int -> Text -> [Text]
wrapLines w t = case greedyGroups T.length 1 w (T.words t) of
  [] -> [" "]
  gs -> map T.unwords gs

-- deterministic per-cell noise keyed on tick swaps chars and occasionally
-- runs a cell 'hot'; a click scrambles an expanding ring
glitchArt :: [Text] -> Int -> [(Int, Int, Int)] -> [[(AttrName, Text)]]
glitchArt art tick ripples =
  [ runs [cell x y c | (x, c) <- zip [0 ..] (T.unpack row)]
  | (y, row) <- zip [0 ..] art
  ]
  where
    -- ring strength at a cell: 0 = none, ~1 = fresh; fades to 0 over frames
    rippleStrength :: Int -> Int -> Double
    rippleStrength x y =
      maximum
        ( 0
            : [ 1 - age / life
              | (rx, ry, t0) <- ripples,
                let age = fromIntegral (tick - t0)
                    dx = fromIntegral (x - rx)
                    dy = fromIntegral (y - ry) * 2 -- char cells are ~2:1
                    d = sqrt (dx * dx + dy * dy),
                abs (d - age * 2.2) < 2.2
              ]
        )
      where
        life = fromIntegral rippleFrames
    cell x y c
      | c == ' ' =
          -- fade quadratically
          let rs = rippleStrength x y
              fade = rs * rs
           in if hash x y < round (30 * fade)
                then (if rs > 0.5 then logoMidAttr else metaAttr, '·')
                else (metaAttr, ' ')
      | otherwise =
          let h = hash x y
              rs = rippleStrength x y
              fade = rs * rs
              gp = 6 + round (55 * fade)
              c' = if h < gp then glitch !! (h `mod` length glitch) else c
              a
                | h < round (40 * fade) = logoHotAttr
                | h < 2 = logoHotAttr
                | c' `elem` ("@%Pqbd█▛▜▙▟▀▄▌▐▘▝▖▗▚▞" :: String) = logoDenseAttr
                | c' `elem` ("#*+=" :: String) = logoMidAttr
                | otherwise = metaAttr
           in (a, c')
    hash x y = noise x y (tick `div` 2) 101
    glitch = "%#*-=:@" :: String

-- group same-attr cells into single txt widgets per run
runs :: [(AttrName, Char)] -> [(AttrName, Text)]
runs = map merge . groupBy (\a b -> fst a == fst b)
  where
    merge [] = (metaAttr, T.empty)
    merge grp@((a, _) : _) = (a, T.pack (map snd grp))

-- horizontal rule that drops the occasional stitch
glitchRule :: Int -> Widget Name
glitchRule tick =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx ^. availWidthL
        ch x =
          let h = noise x 0 (tick `div` 4) 211
           in if h < 2
                then (if even h then '┄' else '╌')
                else '─'
    render (withAttr metaAttr (txt (T.pack (map ch [0 .. w - 1]))))

-- scramble-in: text resolves over burstFrames ticks
scramble :: Int -> Int -> AttrName -> Text -> Widget Name
scramble seed age baseAttr t =
  hBox [withAttr a (txt r) | (a, r) <- runs cells]
  where
    cells =
      [ if hit then (logoHotAttr, "%#*=:@" !! (h `mod` 6)) else (baseAttr, c)
      | (i, c) <- zip [0 ..] (T.unpack t),
        let h = noise i seed age 101
            thr = [88, 55, 30, 12, 4] !! min age 4
            hit = h < thr && c /= ' '
      ]
