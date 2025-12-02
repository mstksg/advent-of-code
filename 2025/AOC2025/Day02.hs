-- |
-- Module      : AOC2025.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day02 (
  day02a,
  day02b,
)
where

import AOC.Common (listTup)
import AOC.Solver (noFail, type (:~>) (..))
import Control.Monad ((<=<))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

-- | repDigits 3 567 = 567567567
repDigits :: Int -> Int -> Int
repDigits n = read . concat . replicate n . show

-- | Up to 1e11
repN :: [Int] -> IntSet
repN = foldMap \n ->
  IS.fromAscList . takeWhile (< 1e11) . map (repDigits n) $ [1 ..]

day02 :: [Int] -> [(Int, Int)] :~> Int
day02 ns =
  MkSol
    { sParse =
        traverse (listTup <=< traverse readMaybe . splitOn "-") . splitOn ","
    , sShow = show
    , sSolve =
        noFail $
          IS.foldl' (+) 0 . foldMap (IS.intersection (repN ns) . IS.fromRange)
    }

day02a :: [(Int, Int)] :~> Int
day02a = day02 [2]

day02b :: [(Int, Int)] :~> Int
day02b = day02 [2 .. 11]
