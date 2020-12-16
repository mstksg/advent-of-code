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
input = [6,13,1,15,2,0]

put :: Ord k => k -> b -> Map k [b] -> Map k [b]
put k v m = Map.insert k (take 2 $ v : Map.findWithDefault [] k m) m

part1 = go (start input)
  where
    start input = (pure <$> Map.fromList (zip input [0..]), last input, length input)
    go (times :: Map Int [Int], num, 2020) = num
    go (times, num, i) = go (put nextnum i times, nextnum, i+1)
      where
        nextnum = case Map.lookup num times of
          Just (a:b:_) -> a-b
          _ -> 0

part2 = go (pure <$> Map.fromList (zip input [0..])) (last input) (length input)
  where
    go !times !num 30000000 = num
    go !times !num i = go (put nextnum i times) nextnum (i+1)
      where
        nextnum = case Map.lookup num times of
          Just (a:b:_) -> a-b
          _ -> 0

main = print part2
