{-# LANGUAGE ConstraintKinds #-}
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

input :: [(Int, Int, Char, Text)]
input = unsafePerformIO load
  where
    load = parse p <$> T.readFile "input/02.txt"
    p = some $ do
      a <- p_nat
      char '-'
      b <- p_nat
      spaces
      cc <- anyChar
      text ": "
      msg <- letters
      spaces
      return (a, b, cc, msg)

part1 :: Int
part1 = sum [1 | x <- input, val x]
  where
    val (a, b, c, msg) = let n = count c (T.unpack msg) in
                           a <= n && n <= b

part2 :: Int
part2 = sum [1 | x <- input, val x]
  where
    val (a, b, c, msg) = count c [msg T.! (a-1),
                                  msg T.! (b-1)] == 1
