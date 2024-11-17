-- |
-- Module      : AOC2016.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
module AOC2016.Day06 (
  day06a,
  day06b,
) where

import AOC.Common (freqs, maximumVal, minimumVal)
import AOC.Solver ((:~>) (..))
import Data.List (transpose)

day06a :: [String] :~> String
day06a =
  MkSol
    { sParse = Just . lines
    , sShow = id
    , sSolve = traverse (fmap fst . maximumVal . freqs) . transpose
    }

day06b :: [String] :~> String
day06b =
  MkSol
    { sParse = Just . lines
    , sShow = id
    , sSolve = traverse (fmap fst . minimumVal . freqs) . transpose
    }
