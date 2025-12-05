-- |
-- Module      : AOC2025.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day05 (
  day05a,
  day05b,
)
where

import AOC.Common (countTrue, listTup)
import AOC.Solver (noFail, (:~>) (..))
import Control.Monad ((<=<))
import Data.ExtendedReal (Extended (..))
import qualified Data.Interval as I
import qualified Data.IntervalSet as IVS
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

day05a :: ([(Int, Int)], [Int]) :~> Int
day05a =
  MkSol
    { sParse = \xs -> do
        [a, b] <- pure $ splitOn "\n\n" xs
        as <- traverse (listTup <=< traverse readMaybe . splitOn "-") $ lines a
        bs <- traverse readMaybe $ lines b
        pure (as, bs)
    , sShow = show
    , sSolve =
        noFail \(ranges, xs) ->
          let allRanges :: IVS.IntervalSet Int
              allRanges = foldMap (\(x, y) -> IVS.singleton $ Finite x I.<=..<= Finite y) ranges
           in countTrue (`IVS.member` allRanges) xs
    }

day05b :: [(Int, Int)] :~> Int
day05b =
  MkSol
    { sParse = fmap fst . sParse day05a
    , sShow = show
    , sSolve =
        noFail $
          sum
            . map ((+ 1) . I.width)
            . IVS.toAscList
            . foldMap (\(x, y) -> IVS.singleton $ Finite x I.<=..<= Finite y)
    }
