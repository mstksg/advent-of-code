-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
module AOC2023.Day06 (
  day06a,
  day06b,
)
where

import AOC.Common (listTup)
import AOC.Solver (noFail, (:~>) (..))
import Control.Monad ((<=<))
import Data.Char (isDigit)
import Data.List (transpose)
import Text.Read (readMaybe)

getWays :: Int -> Int -> Int
getWays tMax dRecord = isqrt (tMax * tMax - 4 * dRecord - 2)

-- | integer square root
isqrt :: Int -> Int
isqrt n
  | n < 2 = n
  | otherwise = go n
  where
    go !x
      | x == y = x
      | otherwise = go y
      where
        y = (x + n `div` x) `div` 2

day06a :: [(Int, Int)] :~> Int
day06a =
  MkSol
    { sParse =
        traverse listTup
          . transpose
          <=< traverse (traverse readMaybe . drop 1 . words)
            . lines
    , sShow = show
    , sSolve = noFail $ product . map (uncurry getWays)
    }

day06b :: (Int, Int) :~> Int
day06b =
  MkSol
    { sParse = listTup <=< traverse (readMaybe . filter isDigit) . lines
    , sShow = show
    , sSolve = noFail $ uncurry getWays
    }
