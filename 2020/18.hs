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
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import qualified Util.Text as T

input :: [Text]
input = unsafePerformIO (T.lines <$> T.readFile "input/18.txt")

part1 :: Int
part1 = sum (map (parse p) input)
  where
    item = p_nat <|> (char '(' *> spaces *> p <* spaces <* char ')')
    k a = (do
      op <- oneOf "*+" <* spaces
      b <- item <* spaces
      case op of
        '+' -> k (a+b)
        '*' -> k (a*b)) <|> pure a
    p = do
      a <- item
      spaces
      k a

part2 :: Int
part2 = sum (map (parse p) input)
  where
    item = (p_nat <|> (char '(' *> spaces *> p <* char ')')) <* spaces
    mul = product <$> sepBy add (text "* ") <* spaces
    add = sum <$> sepBy item (text "+ ") <* spaces
    p = mul
