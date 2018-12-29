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
import           Data.Functor
import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
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


data V4 a = V4 a a a a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Pos = V4 Int

instance Applicative V4 where
  pure a = V4 a a a a
  (V4 f g h i) <*> (V4 x y z w) = V4 (f x) (g y) (h z) (i w)

instance Num a => Num (V4 a) where
  fromInteger n = pure (fromInteger n)
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs _ = undefined
  signum _ = undefined

norm1 :: Pos -> Int
norm1 v = sum (abs <$> v)

pInput :: CharParsing m => m [Pos]
pInput = some pPos
  where
    pPos = (V4
            <$> number <* char ','
            <*> number <* char ','
            <*> number <* char ','
            <*> number <* spaces)

getConnected :: [Pos] -> Map Pos [Pos]
getConnected xs = Map.fromList [(x, filter (\y -> norm1 (x - y) <= 3) xs) | x <- xs]

findRoot :: Pos -> Map Pos Pos -> Pos
findRoot p tree = if parent == p then
                    p
                  else
                    findRoot parent tree
  where
    parent = tree Map.! p

merge :: Pos -> Pos -> Map Pos Pos -> Map Pos Pos
merge p1 p2 tree
  | r1 == r2 = tree
  | otherwise = Map.insert r1 r2 tree
  where
    r1 = findRoot p1 tree
    r2 = findRoot p2 tree

solve1 :: [Pos] -> Int
solve1 xs = Set.size (Set.fromList [findRoot p tree | p <- xs])
  where
    tree = foldr go (Map.fromList [(x,x) | x <- xs]) (Map.toList conns)
    go (p1, p2s) tree = foldr (\p2 tree -> merge p1 p2 tree) tree p2s
    conns = getConnected xs

main :: IO ()
main = do
  txt <- readFile "input/25.txt"
  let xs = parse pInput txt
  print (solve1 xs)
