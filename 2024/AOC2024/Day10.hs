-- |
-- Module      : AOC2024.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day10 (
  day10a,
  day10b,
)
where

import AOC.Common (decimalDigit)
import AOC.Common.Point (Point, cardinalNeighbsSet, parseAsciiMap)
import AOC.Solver (noFail, type (:~>) (..))
import Control.Lens (preview)
import Data.Finite (Finite)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup (Sum (getSum))
import qualified Data.Set as S

gatherNines :: (Eq a, Enum a, Bounded a, Monoid m) => (Point -> m) -> Map Point a -> Point -> m
gatherNines f mp = go minBound
  where
    go x p
      | x == maxBound = f p
      | otherwise =
          foldMap (go (succ x)) . M.keys . M.filter (== succ x) $ mp `M.restrictKeys` cardinalNeighbsSet p

day10 :: Monoid m => (Point -> m) -> (m -> Int) -> Map Point (Finite 10) :~> Int
day10 gather observe =
  MkSol
    { sParse = noFail $ parseAsciiMap (preview decimalDigit)
    , sShow = show
    , sSolve = noFail \mp -> sum . map (observe . gatherNines gather mp) . M.keys $ M.filter (== 0) mp
    }

day10a :: Map Point (Finite 10) :~> Int
day10a = day10 S.singleton S.size

day10b :: Map Point (Finite 10) :~> Int
day10b = day10 (const 1) getSum
