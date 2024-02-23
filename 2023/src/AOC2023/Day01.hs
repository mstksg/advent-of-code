-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
module AOC2023.Day01 (
  day01a,
  day01b,
)
where

import AOC.Common (digitToIntSafe, firstJust)
import AOC.Solver ((:~>) (..))
import Control.Monad (guard)
import Data.Functor (($>))
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)
import Safe (headMay, lastMay)

firstAndLast :: [Int] -> Maybe Int
firstAndLast xs = (\x y -> x * 10 + y) <$> headMay xs <*> lastMay xs

dictionary :: [(String, Int)]
dictionary =
  [(show y, y) | y <- [0 .. 9]]
    ++ zip
      ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      [0 .. 9]

day01a :: [String] :~> Int
day01a =
  MkSol
    { sParse = Just . lines
    , sShow = show
    , sSolve = Just . sum . mapMaybe (firstAndLast . mapMaybe digitToIntSafe)
    }

day01b :: [String] :~> Int
day01b =
  MkSol
    { sParse = Just . lines
    , sShow = show
    , sSolve = Just . sum . mapMaybe (firstAndLast . mapMaybe hasNumber . tails)
    }
  where
    hasNumber x = firstJust (\(t, y) -> guard (t `isPrefixOf` x) $> y) dictionary
