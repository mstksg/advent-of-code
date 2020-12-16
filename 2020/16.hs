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

data Rule = Rule (Int,Int) (Int,Int)
  deriving (Show)

input :: (Map Text Rule, [Int], [[Int]])
input = unsafePerformIO (parse p <$> T.readFile "input/16.txt")
  where
    p_range = do
      a <- p_nat
      char '-'
      b <- p_nat
      return (a, b)
    p = do
      rules <- some $ do
        name <- someText (letter <|> char ' ')
        text ": "
        r1 <- p_range
        text " or "
        r2 <- p_range
        char '\n'
        return (name, Rule r1 r2)
      spaces
      text "your ticket:"
      spaces
      you <- some $ p_nat <* optional (char ',')
      spaces
      text "nearby tickets:"
      spaces
      nearby <- some $ some (p_nat <* optional (char ',')) <* spaces
      return (Map.fromList rules, you, nearby)

valid num (Rule (a,b) (c,d)) = (a <= num && num <= b) || (c <= num && num <= d)

part1 = sum $ do
  ticket <- nearby
  num <- ticket
  guard (all (not . valid num) rules)
  return num
  where
    (rules, you, nearby) = input
    numbers = fold nearby

choose xs = [(xs!!i, take i xs ++ drop (i+1) xs) | i <- [0..n-1]]
  where n = length xs

part2 = product [you!!j | (i, j) <- Map.toList mapping, T.isPrefixOf "departure" i]
  where
    n = length rules'
    match = Map.fromListWith (<>) $ do
      i <- Map.keys rules
      j <- [0..n-1]
      guard $ all (\ticket -> valid (ticket!!j) (rules Map.! i)) validTickets
      return (i,Set.singleton j)
    go order [] left = return order
    go order (i:names) left = do
      let allowed = Set.intersection (match Map.! i) left
      j <- Set.toList allowed
      go (Map.insert i j order) names (Set.delete j left)
    mapping = head $ go Map.empty (Map.keys rules) (Set.fromList [0..n-1])
    (rules, you, nearby) = input
    rules' = Map.toList rules
    numbers = fold nearby
    invalidTicket ticket = any (\num -> all (not . valid num) rules) ticket
    validTickets = filter (not . invalidTicket) nearby
