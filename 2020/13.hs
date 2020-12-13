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

input :: (Integer, [Maybe Integer])
input = unsafePerformIO (parse p <$> T.readFile "input/13.txt")
  where
    p = do
      t <- p_nat <* spaces
      nums <- some (((Just <$> p_nat) <|> (const Nothing <$> char 'x')) <* optional (char ','))
      return (t, nums)

part1 = minimum [(wait n, wait n * n) | Just n <- ids]
  where
    wait n = mod (n - mod t n) n
    (t, ids) = input

part2 = go rules [0..]
  where
    go [] ts = head ts
    go (r:rs) ts = go rs (accelerate (filter (solves r) ts))
    solves (dt,n) t = mod (t+dt) n == 0
    accelerate ts = let a:b:_ = ts in [a,b..]
    rules = [(dt, n) | (dt, Just n) <- zip [0..] (snd input)]
    (_, ids) = input
