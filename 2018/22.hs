#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable
import           Data.Heap (MinHeap)
import qualified Data.Heap as H
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ord
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Generics
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators

import           Util.Util

data Pos = Pos {
  x :: Int,
  y :: Int
  }
  deriving (Show, Eq, Ord)

instance Num Pos where
  fromInteger n = Pos (fromInteger n) (fromInteger n)
  Pos x1 y1 + Pos x2 y2 = Pos (x1 + x2) (y1 + y2)
  Pos x1 y1 - Pos x2 y2 = Pos (x1 - x2) (y1 - y2)
  Pos x1 y1 * Pos x2 y2 = Pos (x1 * x2) (y1 * y2)
  abs _ = undefined
  signum _ = undefined

memoize :: Pos -> (Pos -> a) -> (Pos -> a)
memoize (Pos xmax ymax) f = \(Pos x y) -> cache V.! y V.! x
  where
    cache = V.generate (ymax+1) $
            \y -> V.generate (xmax+1) $
            \x -> f (Pos x y)

calcTerrain :: (Int, Pos) -> Pos -> Int
calcTerrain (depth, target) = terrain
  where
    geologicIndex :: Pos -> Int
    geologicIndex = memoize (target+1000) calcIndex
    calcIndex (Pos 0 0) = 0
    calcIndex p | p == target = 0
    calcIndex (Pos x 0) = x * 16807
    calcIndex (Pos 0 y) = y * 48271
    calcIndex (Pos x y) = erosion (Pos (x-1) y) * erosion (Pos x (y-1))

    erosion p = mod (geologicIndex p + depth) 20183
    terrain p = erosion p `mod` 3


solve1 :: Int
solve1 = sum [terrain (Pos px py) | py <- [0..y target], px <- [0..x target]]
  where
    terrain = calcTerrain (depth, target)

    -- Puzzle input
    depth = 8103
    target = Pos 9 758
    -- -- Example input
    -- depth = 510
    -- target = Pos 10 10

-- Returns all reachable goals by cost
astar :: forall s. (Ord s) => (s -> [s]) -> (s -> s -> Int) -> (s -> Int) -> s -> Int
astar actions cost goal s0 = go Set.empty (H.singleton (goal s0, (s0, 0)))
  where
    go :: Set s -> MinHeap (Int, (s, Int)) -> Int
    go visited frontier =
      case H.view frontier of
        Just ((_, (s, d)), frontier')
          | Set.member s visited -> go visited frontier'
          | goal s == 0 ->  d
          | otherwise   ->  let new = H.fromList [let d' = d + cost s s'
                                                  in (d' + goal s', (s', d'))
                                                 | s' <- actions s]
                            in go visited' (frontier' <> new)
            where
              visited' = Set.insert s visited
        Nothing -> error "no route"

data Equip = Torch | Climbing | Neither
  deriving (Show, Eq, Ord)

allows :: Int -> Equip -> Bool
allows 0 Neither = False
allows 1 Torch = False
allows 2 Climbing = False
allows _ _ = True

neighbors :: Pos -> [Pos]
neighbors (Pos x y) = [Pos (x+1) y, Pos (x-1) y,
                       Pos x (y+1), Pos x (y-1)]

norm1 :: Pos -> Int
norm1 (Pos x y) = abs x + abs y

solve2 :: Int
solve2 = astar actions cost goal (0, Torch)
  where
    terrain = calcTerrain (depth, target)

    actions (p1, e1) = ( -- move
      [(p2, e1) | p2 <- neighbors p1,
        x p2 >= 0, y p2 >= 0,
        terrain p2 `allows` e1] ++
        -- change equipment
      [(p1, e2) | e2 <- [Torch, Climbing, Neither],
        e2 /= e1,
        terrain p1 `allows` e2])

    cost (p1,e1) (p2,e2)
      | p2 /= p1 = 1
      | e2 /= e1 = 7

    goal (p, e)
      | p == target && e == Torch = 0
      | otherwise = norm1 (p - target) + (if e /= Torch then 7 else 0)

    -- Puzzle input
    depth = 8103
    target = Pos 9 758

    -- -- Example input
    -- depth = 510
    -- target = Pos 10 10


main :: IO ()
main = do
  print solve1
  print solve2
