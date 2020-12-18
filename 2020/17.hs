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
import           Control.Monad.Trans.State
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
import qualified Ersatz as E
import qualified Ersatz.Solver.Minisat as E
import           Linear.V3
import           Linear.V4
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import qualified Util.Text as T

input :: [String]
input = unsafePerformIO (lines . T.unpack <$> T.readFile "input/17.txt")

neighbors :: V3 Int -> [V3 Int]
neighbors p = [p + d | d <- sequence (pure [-1,0,1]), sum (abs <$> d) >= 1]

neighbors4 :: V4 Int -> [V4 Int]
neighbors4 p = [p + d | d <- sequence (pure [-1,0,1]), sum (abs <$> d) >= 1]

part1 :: Int
part1 = Set.size (iterate step start !! 6)
  where
    step grid = Set.fromList [p | p <- toList toCheck, check p]
      where
        check p =
          case (Set.member p grid, count True [Set.member n grid | n <- neighbors p]) of
            (True, 2) -> True
            (True, 3) -> True
            (True, _) -> False
            (False, 3) -> True
            (False, _) -> False
        toCheck = grid <> Set.fromList [n | p <- toList grid, n <- neighbors p]
    start = Set.fromList [V3 x y 0 | (y, line) <- zip [0..] input, (x, '#') <- zip [0..] line]

part2 :: Int
part2 = Set.size (iterate step start !! 6)
  where
    step grid = Set.fromList [p | p <- toList toCheck, check p]
      where
        check p =
          case (Set.member p grid, count True [Set.member n grid | n <- neighbors4 p]) of
            (True, 2) -> True
            (True, 3) -> True
            (True, _) -> False
            (False, 3) -> True
            (False, _) -> False
        toCheck = grid <> Set.fromList [n | p <- toList grid, n <- neighbors4 p]
    start = Set.fromList [V4 x y 0 0 | (y, line) <- zip [0..] input, (x, '#') <- zip [0..] line]
