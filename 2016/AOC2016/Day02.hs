-- |
-- Module      : AOC2016.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
module AOC2016.Day02 (
  day02a,
  day02b,
) where

import AOC.Common.Point (Dir (..), Point, dirPoint, parseDir)
import AOC.Solver ((:~>) (..))
import Data.List (mapAccumL)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Linear (V2 (..))

stepper :: Map Point Char -> Point -> [Dir] -> (Point, Maybe Char)
stepper mp x =
  (\r -> (r, M.lookup r mp))
    . foldl' move x
  where
    move p d
      | p' `M.member` mp = p'
      | otherwise = p
      where
        p' = p + dirPoint d

keypadA :: Map Point Char
keypadA =
  M.fromList . flip zip ['1' ..] $
    [ V2 (-1) 1
    , V2 0 1
    , V2 1 1
    , V2 (-1) 0
    , V2 0 0
    , V2 1 0
    , V2 (-1) (-1)
    , V2 0 (-1)
    , V2 1 (-1)
    ]

day02a :: [[Dir]] :~> String
day02a =
  MkSol
    { sParse = Just . (map . mapMaybe) parseDir . lines
    , sShow = id
    , sSolve = sequence . snd . mapAccumL (stepper keypadA) 0
    }

keypadB :: Map Point Char
keypadB =
  M.fromList . flip zip (['1' .. '9'] ++ ['A' ..]) $
    [ V2 0 2
    , V2 (-1) 1
    , V2 0 1
    , V2 1 1
    , V2 (-2) 0
    , V2 (-1) 0
    , V2 0 0
    , V2 1 0
    , V2 2 0
    , V2 (-1) (-1)
    , V2 0 (-1)
    , V2 1 (-1)
    , V2 0 (-2)
    ]

day02b :: [[Dir]] :~> String
day02b =
  MkSol
    { sParse = Just . (map . mapMaybe) parseDir . lines
    , sShow = id
    , sSolve = sequence . snd . mapAccumL (stepper keypadB) (V2 (-2) 0)
    }
