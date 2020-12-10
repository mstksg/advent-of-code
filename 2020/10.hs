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

input :: [Int]
input = unsafePerformIO (parse p <$> T.readFile "input/10.txt")
  where
    p = some (p_nat <* spaces)

part1 :: Int
part1 = count 1 steps * count 3 steps
  where
    steps = (zipWith(subtract)`ap`tail) (sort (input ++ [0, maximum input + 3]))

splitGT a s = snd (Set.split a s)
splitLT a s = fst (Set.split a s)

reflexive :: Ord a => [a] -> Map a a
reflexive xs = Map.fromList [(x,x) | x <- xs]

part2 :: Integer
part2 = ways 0
  where
    ways_map = ways <$> reflexive [0..end]
    ways i | i >= end = 1
    ways i = sum $ [ways_map Map.! p | p <- Set.toList $ splitLT (i + 4) $ splitGT i $ adapters]
    adapters = Set.fromList (end : input)
    end = maximum input + 3
