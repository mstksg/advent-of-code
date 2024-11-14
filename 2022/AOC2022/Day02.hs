-- |
-- Module      : AOC2022.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day02 (
  day02a,
  day02b,
)
where

import AOC.Common (listTup)
import AOC.Solver ((:~>) (..))
import Data.Finite (Finite, getFinite)
import Data.Maybe (mapMaybe)

type Z3 = Finite 3

matchLetter :: Char -> Maybe Z3
matchLetter = \case
  'A' -> Just 0
  'B' -> Just 1
  'C' -> Just 2
  'X' -> Just 0
  'Y' -> Just 1
  'Z' -> Just 2
  _ -> Nothing

day02 ::
  -- | Get shape score
  (Z3 -> Z3 -> Z3) ->
  -- | Get outcome score
  (Z3 -> Z3 -> Z3) ->
  [(Z3, Z3)] :~> Integer
day02 shapeScore outcomeScore =
  MkSol
    { sParse = traverse parseLine . lines
    , sShow = show
    , sSolve = Just . sum . map go
    }
  where
    parseLine = listTup . mapMaybe matchLetter
    go (x, y) =
      getFinite (shapeScore x y)
        + 1
        + getFinite (outcomeScore x y) * 3

day02a :: [(Z3, Z3)] :~> Integer
day02a = day02 (\_ y -> y) (\x y -> y + (1 - x))

day02b :: [(Z3, Z3)] :~> Integer
day02b = day02 (\x y -> y - (1 - x)) (\_ y -> y)
