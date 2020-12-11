{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Function.Memoize
import           Data.Int
import           Data.List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Debug.Trace
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import qualified Util.Text as T

input :: Vector (Vector Char)
input = unsafePerformIO (parse p <$> T.readFile "input/11.txt")
  where
    p = fmap V.fromList $ some $ do
      V.fromList <$> some (oneOf ".#L") <* spaces

directions = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], abs dx + abs dy > 0]
width = V.length (input V.! 0)
height = V.length input

coordinates :: Vector (Vector (Int,Int))
coordinates = V.fromList [V.fromList [(x,y) | x <- [0..width-1]] | y <- [0..height-1]]

index grid (x,y)
  | x < 0 || x >= width = '`'
  | y < 0 || y >= height = '`'
  | otherwise = grid V.! y V.! x

part1 = go input
  where
    go grid = let grid' = fmap (stepXY grid) <$> coordinates in
                if grid == grid' then count '#' (fold grid) else go grid'
    stepXY grid p = case (index grid p, count '#' [index grid n | n <- neighbors p]) of
                      ('L', 0) -> '#'
                      ('#', n) | n >= 4 -> 'L'
                      (s, _) -> s
    neighbors (x,y) = [(x+dx, y+dy) | (dx, dy) <- directions]

part2 = go input
  where
    go grid = let grid' = fmap (stepXY grid) <$> coordinates in
                if grid == grid' then count '#' (fold grid) else go grid'
    stepXY grid p = case (index grid p, count '#' [index grid n | n <- neighbors p]) of
                      ('L', 0) -> '#'
                      ('#', n) | n >= 5 -> 'L'
                      (s, _) -> s
    neighbors_map = fmap neighbor <$> coordinates
      where neighbor (x,y) = [head [(x', y')
                                   | n <- [1..], let (x', y') = (x+n*dx, y+n*dy),
                                                     index input (x', y') /= '.']
                             | (dx, dy) <- directions]
    neighbors (x,y) = neighbors_map V.! y V.! x

main :: IO ()
main = print part1 >> print part2
