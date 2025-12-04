-- |
-- Module      : AOC2025.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day04 (
  day04a,
  day04b,
)
where

import AOC.Common.Point (Point, fullNeighbsSet, parseAsciiSet)
import AOC.Solver (noFail, (:~>) (..))
import Data.Foldable (fold)
import Data.List (unfoldr)
import Data.Set (Set)
import qualified Data.Set as S

reachable :: Set Point -> Set Point
reachable pts = flip S.filter pts \pt ->
  S.size (fullNeighbsSet pt `S.intersection` pts) < 4

day04a :: Set Point :~> Int
day04a =
  MkSol
    { sParse = noFail $ parseAsciiSet (== '@')
    , sShow = show
    , sSolve = noFail $ S.size . reachable
    }

day04b :: Set Point :~> Int
day04b =
  MkSol
    { sParse = sParse day04a
    , sShow = show
    , sSolve =
        noFail $
          S.size . fold . takeWhile (not . S.null) . unfoldr (Just . go)
    }
  where
    go pts = (removed, pts `S.difference` removed)
      where
        removed = reachable pts
