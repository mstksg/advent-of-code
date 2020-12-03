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

input :: [Text]
input = unsafePerformIO (T.lines <$> T.readFile "input/03.txt")

part1 = sum [if pt (y * 3) y == '#' then 1 else 0
            | y <- [0..height-1]]
  where
    height = length input :: Int
    width = T.length (input !! 0)
    pt x y = input !! y T.! (x `mod` width)

part2 = product (map slope [(1,1), (3,1), (5,1), (7,1), (1,2)])
  where
    height = length input :: Int
    width = T.length (input !! 0)
    pt x y = input !! y T.! (x `mod` width)
    slope (dx, dy) = sum [if pt (div y dy * dx) y == '#' then 1 else 0
                         | y <- [0,dy..height-1]]
