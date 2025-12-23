-- |
-- Module      : AOC2019.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
module AOC2019.Day01 (
  day01a,
  day01b,
) where

import AOC.Common (readAll)
import AOC.Solver (noFail, (:~>) (..))

fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)

day01a :: [Int] :~> Int
day01a =
  MkSol
    { sParse = readAll . lines
    , sShow = show
    , sSolve = noFail $ sum . map fuel
    }

day01b :: [Int] :~> Int
day01b =
  MkSol
    { sParse = readAll . lines
    , sShow = show
    , sSolve = noFail $ sum . map (sum . drop 1 . takeWhile (>= 0) . iterate fuel)
    }
