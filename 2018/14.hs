#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.HashTable.IO as H
import           Data.Hashable
import           Data.IORef
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
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
import           System.IO.Unsafe
import           System.Mem.StableName
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Util.Util

step :: (Seq Int, [Int]) -> (Seq Int, [Int])
step (recipes, elves) = (new_recipes, new_elves)
  where
    total = sum [Seq.index recipes i | i <- elves]
    toAdd = [read [d] :: Int | d <- show total]
    new_recipes = recipes <> Seq.fromList toAdd
    new_elves = [let s = Seq.index recipes i in
                   mod (i + s + 1) (Seq.length new_recipes)
                   | i <- elves]

solve1 :: String
solve1 = foldMap show rs
  where
    steps = iterate step (Seq.fromList [3, 7], [0, 1])
    rs = head [Seq.take 10 $ Seq.drop 598701 recipes | (recipes, _) <- steps, Seq.length recipes >= 598701 + 10]

solve2 :: Int
solve2 = go 0 start
  where
    target = Seq.fromList $ [read [c] | c <- show 598701] -- [5,9,8,7,0,1]
    go i (recipes, elves)
      | front == target = i
      | front /= tgt    = go (i+1) (recipes, elves)
      | otherwise       = go i (step (recipes, elves))
      where
        front = Seq.take (Seq.length target) (Seq.drop i recipes)
        tgt   = Seq.take (Seq.length front) target
    start = (Seq.fromList [3, 7], [0, 1])

main :: IO ()
main = do
  print solve1
  print solve2
