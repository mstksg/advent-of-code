#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveTraversable #-}
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
import           Text.Parser.Combinators hiding (count)

import           Util.Util

data V3 a = V3 {
  x :: a,
  y :: a,
  z :: a
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Pos = V3 Int

instance Applicative V3 where
  pure a = V3 a a a
  (V3 f g h) <*> (V3 x y z) = V3 (f x) (g y) (h z)

instance Num a => Num (V3 a) where
  fromInteger n = pure (fromInteger n)
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs _ = undefined
  signum _ = undefined

norm1 :: Pos -> Int
norm1 (V3 x y z) = abs x + abs y + abs z

pInput :: CharParsing m => m [(Pos, Int)]
pInput = some pLine
  where
    pLine = (,) <$> (string "pos=" *> pPos) <* string "," <* spaces
            <* string "r=" <*> number <* spaces
    pPos = between (char '<') (char '>') $
           (V3
            <$> number <* char ','
            <*> number <* char ','
            <*> number)

count :: (a -> Bool) -> [a] -> Int
count p xs = length [x | x <- xs, p x]

solve1 :: [(Pos, Int)] -> Int
solve1 bots = count (inRange strongest) bots
  where
    strongest = maximumOn snd bots
    inRange (p1, r1) (p2, _) = norm1 (p2 - p1) <= r1

data Rect = Rect {
  rectPos :: Pos,
  rectSize :: Int
  }
  deriving (Show, Eq, Ord)

sub :: Rect -> [Rect]
sub (Rect p 1) = []
sub (Rect p r) = [Rect (p+dp) m | dp <- [
                     V3 0 0 0,
                     V3 m 0 0,
                     V3 0 m 0,
                     V3 0 0 m,
                     V3 0 m m,
                     V3 m 0 m,
                     V3 m m 0,
                     V3 m m m
                     ]]
  where
    m = div r 2

nearestLine :: Int -> Int -> Int -> Int
nearestLine x1 x2 x
  | x < x1 = x1
  | x > x2 = x2
  | otherwise = x

furthestLine :: Int -> Int -> Int -> Int
furthestLine x1 x2 x
  | abs (x - x1) > abs (x - x2) = x1
  | otherwise                   = x2

nearest :: Rect -> Pos -> Pos
nearest (Rect rp rs) (V3 px py pz) = V3 mx my mz
  where
    mx = nearestLine (x rp) (x rp + rs - 1) px
    my = nearestLine (y rp) (y rp + rs - 1) py
    mz = nearestLine (z rp) (z rp + rs - 1) pz

furthest :: Rect -> Pos -> Pos
furthest (Rect rp rs) (V3 px py pz) = V3 mx my mz
  where
    mx = furthestLine (x rp) (x rp + rs - 1) px
    my = furthestLine (y rp) (y rp + rs - 1) py
    mz = furthestLine (z rp) (z rp + rs - 1) pz

data Cube = Cube {
  cubeRect :: Rect,
  cubeUpperBound :: Int,
  cubeLowerBound :: Int,
  cubeChildren :: [Cube]
  }
  deriving (Show)

makeCube :: [(Pos, Int)] -> Rect -> Cube
makeCube bots rect
  | rectSize rect == 1 = let c = count (inRange (rectPos rect)) bots
                         in Cube rect c c []
  | otherwise = Cube rect upper lower [makeCube bots c | c <- sub rect]
  where
    inRange p (pb, rb) = norm1 (pb - p) <= rb
    upper = count touching bots
    touching (pb, rb) = norm1 (pb - nearest rect pb) <= rb
    lower = count containing bots
    containing (pb, rb) = norm1 (pb - furthest rect pb) <= rb

update :: Cube -> Cube
update cube
  | cubeUpperBound cube == cubeLowerBound cube = cube
  | otherwise = cube {
      cubeUpperBound = maximum (map cubeUpperBound children'),
      cubeLowerBound = maximum (map cubeLowerBound children'),
      cubeChildren = children'
      }
  where
    children = cubeChildren cube
    i = snd $ maximum [(cubeUpperBound c, j) | (j, c) <- zip [0..] children]
    c = children !! i
    c' = update c
    children' = take i children ++ [c'] ++ drop (i+1) children

updateall :: Cube -> Cube
updateall cube
  | cubeUpperBound cube == cubeLowerBound cube = cube
  | otherwise = updateall (update cube)

maximumsOn :: Ord b => (a -> b) -> [a] -> [a]
maximumsOn f xs = [x | x <- xs, f x == m]
  where
    m = maximum [f x | x <- xs]

filterBest :: Cube -> Cube
filterBest = go . go
  where
    go cube
      | cubeUpperBound cube > cubeLowerBound cube = go (updateall cube)
      | null (cubeChildren cube) = cube
      | otherwise = cube { cubeChildren = (map go viable) }
      where
        viable = maximumsOn cubeUpperBound (cubeChildren cube)


solve2 :: Cube -> Int
solve2 cube = go cube'
  where
    cube' = filterBest cube
    go cube
      | null (cubeChildren cube) = norm1 (rectPos (cubeRect cube))
      | otherwise = minimum [go c | c <- cubeChildren cube]


main :: IO ()
main = do
  txt <- readFile "input/23.txt"
  let bots = parse pInput txt
  putStrLn $ "Number of bots: " ++ show (length bots)
  putStrLn $ "solve1: " ++ show (solve1 bots)
  let minCorner = foldr1 (liftA2 min) (map fst bots)
      maxCorner = foldr1 (liftA2 max) (map fst bots)
      sz = head [2^n | n <- [0..], 2^n > (maximum $ maxCorner - minCorner)]
      cube = makeCube bots (Rect minCorner sz)

  print (solve2 cube)
