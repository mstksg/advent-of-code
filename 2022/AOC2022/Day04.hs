-- |
-- Module      : AOC2022.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day04 (
  day04a,
  day04b,
)
where

import AOC.Common (countTrue, listTup, listV2, readAll)
import AOC.Solver (noFail, (:~>) (..))
import Control.Monad ((<=<))
import qualified Data.ExtendedReal as E
import Data.IntegerInterval (IntegerInterval, (<=..<=))
import qualified Data.IntegerInterval as I
import Data.List.Split (splitOn)
import Linear.V2 (V2 (..))

parseIntervals :: String -> Maybe (V2 IntegerInterval)
parseIntervals =
  traverse (fmap mkInterval . listTup <=< readAll . splitOn "-")
    <=< listV2 . splitOn ","
  where
    mkInterval (x, y) = E.Finite x <=..<= E.Finite y

day04 ::
  (V2 IntegerInterval -> Bool) ->
  [V2 IntegerInterval] :~> Int
day04 f =
  MkSol
    { sParse = traverse parseIntervals . lines
    , sShow = show
    , sSolve = noFail $ countTrue f
    }

day04a :: [V2 IntegerInterval] :~> Int
day04a = day04 \(V2 xs ys) -> xs `I.isSubsetOf` ys || ys `I.isSubsetOf` xs

day04b :: [V2 IntegerInterval] :~> Int
day04b = day04 \(V2 xs ys) -> xs I.==? ys
