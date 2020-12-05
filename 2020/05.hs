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

input :: [String]
input = unsafePerformIO (map T.unpack . T.lines <$> T.readFile "input/05.txt")

dec c [] = c
dec c ('F':xs) = dec (2*c) xs
dec c ('B':xs) = dec (2*c+1) xs
dec c ('L':xs) = dec (2*c) xs
dec c ('R':xs) = dec (2*c+1) xs

seat :: String -> Int
seat bsp = dec 0 bsp

part1 :: Int
part1 = maximum (map seat input)

part2 :: (Int,Int)
part2 = head . dropWhile (\(a,b) -> b-a==1) . (zip`ap`tail) . sort $ map seat input
