-- |
-- Module      : AOC2024.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day11 (
  day11a,
  day11b,
)
where

import AOC.Common (numDigits, _ListTup)
import AOC.Common.Parser (pDecimal, parseMaybe')
import AOC.Solver (noFail, type (:~>) (..))
import Control.Applicative (Alternative (many))
import Control.Lens (review)
import qualified Data.MemoCombinators as Memo

day11 :: Int -> [Int] :~> Int
day11 n =
  MkSol
    { sParse = parseMaybe' $ many pDecimal
    , sShow = show
    , sSolve = noFail $ sum . map (`growFrom` n)
    }

day11a :: [Int] :~> Int
day11a = day11 25

day11b :: [Int] :~> Int
day11b = day11 75

growFrom :: Int -> Int -> Int
growFrom = Memo.memo2 Memo.integral Memo.integral go
  where
    go _ 0 = 1
    go n k = sum . map (`growFrom` (k - 1)) $ step n
    step c
      | c == 0 = [1]
      | even pow = review _ListTup $ c `divMod` (10 ^ (pow `div` 2))
      | otherwise = [c * 2024]
      where
        pow = numDigits c
