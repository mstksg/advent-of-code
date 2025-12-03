-- |
-- Module      : AOC2025.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day03 (
  day03a,
  day03b,
)
where

import AOC.Solver (noFail, (:~>) (..))
import Control.Monad (replicateM, (<=<))
import Control.Monad.Trans.State (StateT (StateT), evalStateT)
import Data.Char (digitToInt, intToDigit)
import Data.List (tails)
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

day03 :: Int -> [[Int]] :~> Int
day03 n =
  MkSol
    { sParse = noFail $ map (map digitToInt) . lines
    , sShow = show
    , sSolve =
        fmap sum . traverse (readMaybe @Int <=< listToMaybe . evalStateT go)
    }
  where
    go :: StateT [Int] [] String
    go = replicateM n (intToDigit <$> StateT descendPicks)

day03a :: [[Int]] :~> Int
day03a = day03 2

day03b :: [[Int]] :~> Int
day03b = day03 12

dropWhileMany :: (a -> Bool) -> [a] -> [[a]]
dropWhileMany p xs =
  [ xs'
  | x : xs' <- tails xs
  , not $ p x
  ]

descendPicks :: [Int] -> [(Int, [Int])]
descendPicks xs = do
  n <- reverse [1 .. 9]
  xs' <- dropWhileMany (/= n) xs
  pure (n, xs')
