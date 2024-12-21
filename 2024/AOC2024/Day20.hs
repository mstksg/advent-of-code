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

import AOC.Common (countTrue, findKeyFor)
import AOC.Common.Point (Point, cardinalNeighbsSet, mannDist, parseAsciiMap)
import AOC.Common.Search
import AOC.Solver (noFail, type (:~>) (..))
import Data.Functor ((<&>))
import Data.List (tails)
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
findCheats walls start end len thresh = do
  path <- (start :) <$> bfs ((`S.difference` walls) . cardinalNeighbsSet) start (== end)
  pure $
    sum $
      tails path <&> \case
        [] -> 0
        p : ps ->
          countTrue
            (\(i, q) -> mannDist p q <= len && i - mannDist p q >= thresh)
            (drop (thresh + 1) $ zip [1 ..] ps)

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
