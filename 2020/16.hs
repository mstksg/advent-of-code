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

valid :: Int -> Rule -> Bool
valid num (Rule (a,b) (c,d)) = (a <= num && num <= b) || (c <= num && num <= d)

part1 :: Int
part1 = sum $ do
  ticket <- nearby
  num <- ticket
  guard (all (not . valid num) rules)
  return num
  where
    (rules, you, nearby) = input
    numbers = fold nearby

fromopts :: [Bool] -> Int
fromopts xs = fst $ head $ filter snd (zip [0..] xs)

part2 :: Int
part2 = product [you!!j | (i, j) <- Map.toList mapping, T.isPrefixOf "departure" i]
  where
    n = length rules
    match = Map.fromListWith (<>) $ do
      i <- Map.keys rules
      j <- [0..n-1]
      guard $ all (\ticket -> valid (ticket!!j) (rules Map.! i)) validTickets
      return (i, Set.singleton j)

    mapping = fmap fromopts $ head $ solves $ do
      let options n = do
            xs <- replicateM n E.exists :: Ersatz [E.Bit]
            E.assert (exactly 1 xs)
            return xs
      mp <- sequence (Map.fromList [(name, options n) | name <- Map.keys rules])
      for_ [1..n-1] $ \j -> do
        E.assert $ exactly 1 [mp Map.! name !! j | name <- Map.keys mp]
      for_ (Map.toList match) $ \(name, js) -> do
        E.assert $ exactly 1 [mp Map.! name !! j | j <- Set.toList js]
      return mp

    (rules, you, nearby) = input
    numbers = fold nearby
    invalidTicket ticket = any (\num -> all (not . valid num) rules) ticket
    validTickets = filter (not . invalidTicket) nearby
