-- |
-- Module      : AOC2022.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day06 (
  day06a,
  day06b,
)
where

import AOC.Common (charFinite, firstRepeatedFinitary, slidingWindows)
import AOC.Solver ((:~>) (..))
import Data.Finite (Finite)
import Data.Foldable (toList)
import Data.List (findIndex)
import Data.Maybe (isNothing, mapMaybe)

day06 :: Int -> [Finite 26] :~> Int
day06 n =
  MkSol
    { sParse = Just . map snd . mapMaybe charFinite
    , sShow = show
    , sSolve =
        fmap (+ n)
          . findIndex (isNothing . firstRepeatedFinitary . toList)
          . slidingWindows n
    }

day06a :: [Finite 26] :~> Int
day06a = day06 4

day06b :: [Finite 26] :~> Int
day06b = day06 14
