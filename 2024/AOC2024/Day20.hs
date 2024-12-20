-- |
-- Module      : AOC2024.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day20 (
  day20a,
  day20b,
)
where

import AOC.Common (findKeyFor, floodFill, floodFillSteps, countTrue)
import AOC.Common.Point (Point, cardinalNeighbsSet, mannDist, parseAsciiMap)
import AOC.Solver (noFail, type (:~>) (..))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as S

findCheats ::
  -- | walls
  Set Point ->
  -- | start
  Point ->
  -- | end
  Point ->
  -- | cheat length
  Int ->
  -- | threshold
  Int ->
  Maybe Int
findCheats walls start end len thresh =
  M.lookup end distFromStart <&> \noCheatDist ->
    sum $
      M.toList distFromStart <&> \(p, n) ->
        countTrue (\(q, m) -> n + m + mannDist p q <= noCheatDist - thresh) $
            M.toList $ M.restrictKeys distFromEnd (S.mapMonotonic (+ p) diamond)
  where
    distFromStart = floodFillSteps ((`S.difference` walls) . cardinalNeighbsSet) (S.singleton start)
    distFromEnd = floodFillSteps ((`S.difference` walls) . cardinalNeighbsSet) (S.singleton end)
    diamond = floodFill (S.filter ((<= len) . mannDist 0) . cardinalNeighbsSet) (cardinalNeighbsSet 0)

day20 :: Int -> Map Point (Maybe Bool) :~> Int
day20 len =
  MkSol
    { sParse =
        noFail $ parseAsciiMap \case
          'S' -> Just $ Just False
          'E' -> Just $ Just True
          '#' -> Just Nothing
          _ -> Nothing
    , sShow = show
    , sSolve = \mp -> do
        start <- findKeyFor (Just False) mp
        end <- findKeyFor (Just True) mp
        let walls = M.keysSet $ M.filter isNothing mp
        findCheats walls start end len 100
    }

day20a :: Map Point (Maybe Bool) :~> Int
day20a = day20 2

day20b :: Map Point (Maybe Bool) :~> Int
day20b = day20 20
