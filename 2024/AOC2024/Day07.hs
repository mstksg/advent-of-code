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
import Control.Monad (guard, mfilter)
import Data.Foldable1 (foldlM1)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)

-- | Operations must be monotonic
search :: [Int -> Int -> Int] -> Int -> NonEmpty Int -> Bool
search operations n = elem n . foldlM1 go
  where
    go a b = mfilter (<= n) $ map (\f -> f a b) operations

-- | Operations must be monotonic
day07 :: [Int -> Int -> Int] -> [(Int, NonEmpty Int)] :~> Int
day07 ops =
  MkSol
    { sParse = parseLines $ (,) <$> pDecimal <* ":" <*> NE.some1 pDecimal
    , sShow = show
    , sSolve = noFail $ sum . mapMaybe (\(n, xs) -> n <$ guard (search ops n xs))
    }

day07a :: [(Int, NonEmpty Int)] :~> Int
day07a = day07 [(+), (*)]

day07b :: [(Int, NonEmpty Int)] :~> Int
day07b = day07 [(+), (*), cat]
  where
    cat x y = x * 10 ^ pow + y
      where
        pow :: Int
        pow = ceiling @Double $ logBase 10 (fromIntegral (y + 1))
