{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : AOC2018.Day10
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
module AOC2018.Day10 (
  day10a,
  day10b,
  centralize,
) where

import AOC.Common (clearOut)
import AOC.Common.Point (parseLettersSafe)
import AOC.Solver ((:~>) (..))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum (..))
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))
import qualified Linear as L

type Point = V2 Double
type Lattice = V2 Int

-- | Shift so that centroid is at zero
centralize :: [Point] -> [Point]
centralize ps = map (subtract mean) ps
  where
    (Sum tot, Sum len) = foldMap (\x -> (Sum x, Sum 1)) ps
    mean = tot L.^/ len

-- | Multiply and find trace
traceMul :: [Point] -> [Point] -> Double
traceMul xs ys = sum $ zipWith L.dot xs ys

findWord ::
  -- | velocities
  [Point] ->
  -- | points
  [Point] ->
  -- | points in word, and # of iterations
  (Set Lattice, Int)
findWord (centralize -> vs) (centralize -> xs) =
  (S.fromList ((map . fmap) round final), round t)
  where
    t = negate $ traceMul xs vs / traceMul vs vs
    final = zipWith (\v x -> x + t L.*^ v) vs xs

day10a :: ([Point], [Point]) :~> Set Lattice
day10a =
  MkSol
    { sParse = fmap unzip . traverse parsePoint . lines
    , sShow = fromMaybe "" . parseLettersSafe
    , sSolve = Just . fst . uncurry findWord
    }

day10b :: ([Point], [Point]) :~> Int
day10b =
  MkSol
    { sParse = fmap unzip . traverse parsePoint . lines
    , sShow = show
    , sSolve = Just . snd . uncurry findWord
    }

parsePoint :: String -> Maybe (Point, Point)
parsePoint xs = case map read . words . clearOut p $ xs of
  [x, y, vx, vy] -> Just (V2 vx vy, V2 x y)
  _ -> Nothing
  where
    p '-' = False
    p c = not $ isDigit c
