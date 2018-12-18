#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           System.IO.Unsafe
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

import           Util.Util

input :: String
input = unsafePerformIO (readFile "input/12.txt")

rules :: Map String String
rules = Map.fromList [(i,o) | [i, o] <- map (splitOn " => ") (lines input)]

initial :: Set Int
initial = Set.fromList [i | (i, c) <- zip [0..] start, c == '#']
  where
    start = splitOn ": " (head (lines input)) !! 1

extend :: Set Int -> Map Int String
extend plants
  | Set.null plants = Map.empty
  | otherwise = Map.fromList [(i, go i) | i <- [Set.findMin plants - 2 .. Set.findMax plants + 2]]
  where
    go i = [if Set.member j plants then '#' else '.' | j <- [i-2 .. i+2]]

step :: Set Int -> Set Int
step plants = Map.keysSet $ Map.filter (\v -> rules Map.! v == "#") $ extend plants

solve1 :: Int
solve1 = sum $ iterate step initial !! 20

solve2 :: Int
solve2 = sum $ iterate step initial !! 50000

main :: IO ()
main = do print solve1
          print solve2
