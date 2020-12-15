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

data Op = Mask String | Mem Int Int
  deriving (Show)

input :: [Op]
input = unsafePerformIO (parse p <$> T.readFile "input/14.txt")
  where
    p = some (
      (do text "mask = "
          bits <- some (oneOf "X01")
          spaces
          return (Mask bits)) <|>
      (do text "mem["
          loc <- p_nat
          text "] = "
          val <- p_nat
          spaces
          return (Mem loc val)))

tobits val = [if testBit val (35-i) then '1' else '0' | i <- [0..35]]
frombits bs = sum [if bs!!i == '1' then shiftL 1 (35-i) else 0 | i <- [0..35]]

masked :: Maybe String -> Int -> Int
masked (Just xs) val = frombits $ zipWith switch xs (tobits val)
  where
    switch 'X' x = x
    switch '1' _ = '1'
    switch '0' _ = '0'

floated :: Maybe String -> Int -> [Int]
floated (Just xs) val = frombits <$> sequence (zipWith switch xs (tobits val))
  where
    switch 'X' _ = ['1', '0']
    switch '1' _ = ['1']
    switch '0' x = [x]

part2 = go Map.empty Nothing input
  where
    go mem mask [] = sum mem
    go mem mask (Mask m : xs) = go mem (Just m) xs
    go mem mask (Mem loc val : xs) =
      go (Map.fromList [(l, val) | l <- floated mask loc] `Map.union` mem) mask xs
