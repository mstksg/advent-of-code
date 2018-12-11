#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           System.IO.Unsafe
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

import           Util.Util

gridSerialNumber :: Int
gridSerialNumber = 2568

hundredsDigit :: Int -> Int
hundredsDigit x = (abs x `div` 100) `mod` 10

powerLevel :: Int -> Int -> Int
powerLevel x y = hundredsDigit part1 - 5
  where
    rackID = x + 10
    part1 = (rackID * y + gridSerialNumber) * rackID



solve1 = maximumOn blockPower [(x, y) | x <- [1..300-2], y <- [1..300-2]]
  where
    blockPower (x,y) = sum [powerLevel px py | px <- [x..x+2], py <- [y..y+2]]

solve2 = maximumOn (\(x,y,r) -> squarePower x y r) [(x,y,r) | x <- [1..300], y <- [1..300], r <- [1..300+1- max x y]]
  where
    squarePower x y r = totalPowerL (x+r) (y+r) - totalPowerL x (y+r) - totalPowerL (x+r) y + totalPowerL x y
    totalPowerL x y = totalPower (x-1) (y-1)

    totalPower x y = totalPower' V.! x V.! y
    totalPower' = V.generate 301 (\x -> V.generate 301 (\y -> totalUp x y))
    totalUp x y
      | x == 0 || y == 0 = 0
      | otherwise        = powerLevel x y + totalPower (x-1) y + totalPower x (y-1) - totalPower (x-1) (y-1)

main :: IO ()
main = do print solve1
          print solve2
