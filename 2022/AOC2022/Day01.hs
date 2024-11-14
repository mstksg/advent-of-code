-- |
-- Module      : AOC2022.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day01 (
  day01a,
  day01b,
)
where

import AOC.Solver ((:~>) (..))
import Data.List (sort)
import Data.List.Split (splitOn)
import Safe.Exact (takeExactMay)
import Safe.Foldable (maximumMay)
import Text.Read (readMaybe)

day01a :: [[Int]] :~> Int
day01a =
  MkSol
    { sParse = traverse (traverse readMaybe . lines) . splitOn "\n\n"
    , sShow = show
    , sSolve = maximumMay . map sum
    }

day01b :: [[Int]] :~> Int
day01b =
  MkSol
    { sParse = sParse day01a
    , sShow = show
    , sSolve = fmap sum . takeExactMay 3 . reverse . sort . map sum
    }
