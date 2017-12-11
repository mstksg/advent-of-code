#!/usr/bin/runhaskell
{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Data.Foldable
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Semigroup
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U

type Memory = Map String Int
type Instruction = Memory -> Memory

parseCondition :: String -> String -> String -> (Memory -> Bool)
parseCondition creg cmps cvals mem = Map.findWithDefault 0 creg mem `cmp` cval
  where
    cmp = case cmps of
            "<" -> (<)
            ">" -> (>)
            "<=" -> (<=)
            ">=" -> (>=)
            "==" -> (==)
            "!=" -> (/=)
    cval = read cvals

parseLine :: String -> Instruction
parseLine line = \mem -> if cond mem then
                           let current = Map.findWithDefault 0 target mem
                           in Map.insert target (modify current) mem
                         else mem
  where
    [target, act, amounts, "if", creg, cmp, cval] = words line
    cond = parseCondition creg cmp cval
    amount = read amounts
    modify = case act of
      "inc" -> (+amount)
      "dec" -> (subtract amount)

parseText :: String -> [Instruction]
parseText text = map parseLine (lines text)

main :: IO ()
main = do
  ins <- parseText <$> readFile "input/08"
  print $ foldMap Max $ foldr (\f fs -> fs . f) id ins Map.empty
  print $ foldMap (foldMap Max) $ scanl (\m f -> f m) Map.empty ins
