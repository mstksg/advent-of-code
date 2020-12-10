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
input = unsafePerformIO (parse p <$> T.readFile "input/09.txt")
  where
    p = some $ p_nat <* spaces

part1 = go (take 25 input) (drop 25 input)
  where
    go pre [] = error "none"
    go pre (x:xs) = case [(a,b) | (a:as) <- tails pre, b <- as, a+b == x] of
                      [] -> x
                      _ -> go (tail pre ++ [x]) xs

part2 = maximum range + minimum range
  where
    target = part1
    sums = scanl' (+) 0 input
    range = drop i (take j input)
    (i, j) = head $ do
      (i, a:as) <- zip [0..] (tails sums)
      (j, b) <- zip [i+1..] as
      guard (b - a == target)
      guard (j - i > 1)
      return (i, j)
