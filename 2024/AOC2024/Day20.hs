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

import AOC.Common (findKeyFor, floodFill)
import AOC.Common.Point (Point, cardinalNeighbsSet, mannDist, mannNorm, parseAsciiMap)
import AOC.Common.Search (bfs)
import AOC.Solver (noFail, type (:~>) (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (mapAccumR)
import Data.Tuple.Strict (T2 (..))

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
  pure . sum . snd $ mapAccumR go (T2 0 M.empty) path
  where
    go :: T2 Int (Map Point Int) -> Point -> (T2 Int (Map Point Int), Int)
    go (T2 i xs) x =
      ( T2 (i + 1) (M.insert x i xs)
      , M.size $
          M.filterWithKey (\y j -> i - j - mannDist x y >= thresh) $
            xs `M.restrictKeys` S.mapMonotonic (+ x) diamond
      )
    diamond = floodFill (S.filter ((<= len) . mannNorm) . cardinalNeighbsSet) (S.singleton 0)

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
