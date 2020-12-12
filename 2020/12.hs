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
import           Linear.V2
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import qualified Util.Text as T

input :: [(Char, Int)]
input = unsafePerformIO (parse p <$> T.readFile "input/12.txt")
  where
    p = some $ do
      (,) <$> letter <*> p_nat <* spaces

rotd :: Int -> V2 Int -> V2 Int
rotd d (V2 x y) = V2 (x * cosd - y * sind) (x * sind + y * cosd)
  where
    cosd = round (cos (fromIntegral d * pi / 180))
    sind = round (sin (fromIntegral d * pi / 180))

manhatten :: V2 Int -> Int
manhatten v = sum (abs <$> v)

part1 =  manhatten . fst $ foldl go (V2 0 0, V2 1 0) input
  where
    go (pos,dir) ('N', d) = (pos + V2 0 d, dir)
    go (pos,dir) ('E', d) = (pos + V2 d 0, dir)
    go (pos,dir) ('S', d) = (pos + V2 0 (-d), dir)
    go (pos,dir) ('W', d) = (pos + V2 (-d) 0, dir)
    go (pos,dir) ('F', d) = (pos + fmap (d*) dir, dir)
    go (pos,dir) ('L', d) = (pos, rotd d dir)
    go (pos,dir) ('R', d) = (pos, rotd (-d) dir)

part2 = manhatten . fst $ foldl go (V2 0 0, V2 10 1) input
  where
    go (pos,wayp) ('N', d) = (pos, wayp + V2 0 d)
    go (pos,wayp) ('E', d) = (pos, wayp + V2 d 0)
    go (pos,wayp) ('S', d) = (pos, wayp + V2 0 (-d))
    go (pos,wayp) ('W', d) = (pos, wayp + V2 (-d) 0)
    go (pos,wayp) ('F', d) = (pos + fmap (d*) wayp, wayp)
    go (pos,wayp) ('L', d) = (pos, rotd d wayp)
    go (pos,wayp) ('R', d) = (pos, rotd (-d) wayp)
