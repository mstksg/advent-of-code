-- |
-- Module      : AOC2021.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
module AOC2021.Day01 (
  day01a,
  day01b,
) where

import AOC.Common (countTrue, laggedPairs, readAll)
import AOC.Solver ((:~>) (..))

parseInput :: String -> Maybe [Int]
parseInput = readAll . lines

countIncreases :: Int -> [Int] -> Int
countIncreases n = countTrue (uncurry (<)) . laggedPairs n

day01a :: [Int] :~> Int
day01a =
  MkSol
    { sParse = parseInput
    , sShow = show
    , sSolve = Just . countIncreases 1
    }

day01b :: [Int] :~> Int
day01b =
  MkSol
    { sParse = parseInput
    , sShow = show
    , sSolve = Just . countIncreases 3
    }
