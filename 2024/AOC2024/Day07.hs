-- |
-- Module      : AOC2024.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day07 (
  day07a,
  day07b,
)
where

import AOC.Common.Parser (pDecimal, parseLines)
import AOC.Solver (noFail, type (:~>) (..))
import Control.Monad (guard)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)

-- | Operations must be monotonic
search :: [Int -> Int -> Maybe Int] -> Int -> NonEmpty Int -> Bool
search operations n (x :| xs) = x `elem` foldrM go n xs
  where
    go a b = mapMaybe (\f -> f a b) operations

-- | Operations must be monotonic
day07 :: [Int -> Int -> Maybe Int] -> [(Int, NonEmpty Int)] :~> Int
day07 ops =
  MkSol
    { sParse = parseLines $ (,) <$> pDecimal <* ":" <*> NE.some1 pDecimal
    , sShow = show
    , sSolve = noFail $ sum . mapMaybe (\(n, xs) -> n <$ guard (search ops n xs))
    }

unAdd :: Int -> Int -> Maybe Int
unAdd x y = [y - x | y >= x]

unMul :: Int -> Int -> Maybe Int
unMul x y = [y `div` x | y `mod` x == 0]

unCat :: Int -> Int -> Maybe Int
unCat x y = [y `div` fac | y `mod` fac == x]
  where
    pow :: Int
    pow = ceiling @Double $ logBase 10 (fromIntegral (x + 1))
    fac = 10 ^ pow

day07a :: [(Int, NonEmpty Int)] :~> Int
day07a = day07 [unAdd, unMul]

day07b :: [(Int, NonEmpty Int)] :~> Int
day07b = day07 [unAdd, unMul, unCat]
