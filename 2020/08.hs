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

data Inst = Acc Int | Jmp Int | Nop Int
  deriving (Show, Eq, Ord)

input :: [Inst]
input = unsafePerformIO (parse p <$> T.readFile "input/08.txt")
  where
    p = some $ do
      (text "acc " *> (Acc <$> p_int) <* spaces)
        <|>
        (text "jmp " *> (Jmp <$> p_int) <* spaces)
        <|>
        (text "nop " *> (Nop <$> p_int) <* spaces)

data End = Loop Int | Term Int | Throw

part1 :: Int
part1 = go 0 0 Set.empty
  where
    go ip acc visited
      | Set.member ip visited = acc
      | otherwise = case input !! ip of
          Acc n -> go (ip + 1) (acc + n) (Set.insert ip visited)
          Jmp n -> go (ip + n) acc (Set.insert ip visited)
          Nop n -> go (ip + 1) acc (Set.insert ip visited)

part2 :: [Int]
part2 = [acc | flip <- [0..length input - 1], Term acc <- [go flip 0 0 Set.empty]]
  where
    go flip ip acc visited
      | Set.member ip visited = Loop acc
      | ip == length input     = Term acc
      | ip < 0 || ip > length input     = Throw
      | ip == flip = case input !! ip of
          Acc n -> go flip (ip + 1) (acc + n) (Set.insert ip visited)
          Nop n -> go flip (ip + n) acc (Set.insert ip visited)
          Jmp n -> go flip (ip + 1) acc (Set.insert ip visited)
      | otherwise = case input !! ip of
          Acc n -> go flip (ip + 1) (acc + n) (Set.insert ip visited)
          Jmp n -> go flip (ip + n) acc (Set.insert ip visited)
          Nop n -> go flip (ip + 1) acc (Set.insert ip visited)
