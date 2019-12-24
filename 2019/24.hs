{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.FiniteField.PrimeField
import           Data.Foldable
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Debug.Trace
import           Linear.Matrix
import           Linear.V2
import           Linear.V3
import           System.IO.Unsafe

import           AStar
import           Util

type Pos = V3 Int -- x, y, level

parseInput :: String -> Set Pos
parseInput txt = Set.fromList [V3 x y 0 | (y, line) <- zip [0..] ls,
                               (x, '#') <- zip [0..] line]
  where
    ls = lines txt



neighbors1 :: V3 Int -> [V3 Int]
neighbors1 p = filter inBounds [p + V3 dx dy 0 | dx <- [-1, 0, 1], dy <- [-1, 0, 1],
                                abs dx + abs dy == 1]
  where
    inBounds (V3 x y _) = 0 <= min x y && max x y <= 4

neighbors2 :: V3 Int -> [V3 Int]
neighbors2 p = fold [warp p (p + V3 dx dy 0) | dx <- [-1, 0, 1], dy <- [-1, 0, 1],
                     abs dx + abs dy == 1]
  where
    warp _ (V3 (-1) y l) = [V3 1 2 (l+1)]
    warp _ (V3    5 y l) = [V3 3 2 (l+1)]
    warp _ (V3 x (-1) l) = [V3 2 1 (l+1)]
    warp _ (V3 x    5 l) = [V3 2 3 (l+1)]
    warp (V3 1 2 l) (V3 2 2 _) = [V3 0 y (l-1) | y <- [0..4]]
    warp (V3 2 1 l) (V3 2 2 _) = [V3 x 0 (l-1) | x <- [0..4]]
    warp (V3 3 2 l) (V3 2 2 _) = [V3 4 y (l-1) | y <- [0..4]]
    warp (V3 2 3 l) (V3 2 2 _) = [V3 x 4 (l-1) | x <- [0..4]]
    warp _ p = [p]

step :: (Pos -> [Pos]) -> Set Pos -> Set Pos
step neighbors grid = Set.filter f $ Set.fromList [p | p0 <- Set.toList grid, p <- neighbors p0]
  where
    bugCount p0 = count True [Set.member p grid | p <- neighbors p0]
    f p0 | Set.member p0 grid = bugCount p0 == 1
         | otherwise = elem (bugCount p0) [1, 2]

solve1 :: Set Pos -> Int
solve1 grid = go grid Set.empty
  where
    go grid seen
      | Set.member grid seen = sum [if Set.member p grid then 2^i else 0 | (i, p) <- zip [0..] level0]
      | otherwise = go (step neighbors1 grid) (Set.insert grid seen)
    level0 = [V3 x y 0 | y <- [0..4], x <- [0..4]]

solve2 :: Set Pos -> Int
solve2 grid = Set.size (iterate (step neighbors2) grid !! 200)
