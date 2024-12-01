-- |
-- Module      : AOC2024.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day01 (
  day01a,
  day01b,
)
where

import AOC.Common (freqs)
import AOC.Common.Parser (pDecimal, parseLines)
import AOC.Solver (noFail, type (:~>) (..))
import Data.List (sort)
import qualified Data.Map as M

day01a :: ([Int], [Int]) :~> Int
day01a =
  MkSol
    { sParse = fmap unzip . parseLines ((,) <$> pDecimal <*> pDecimal)
    , sShow = show
    , sSolve =
        noFail \(xs, ys) ->
          sum . map abs $ zipWith subtract (sort xs) (sort ys)
    }

day01b :: ([Int], [Int]) :~> Int
day01b =
  MkSol
    { sParse = sParse day01a
    , sShow = show
    , sSolve = noFail \(xs, ys) ->
        sum $ map (\x -> x * M.findWithDefault 0 x (freqs ys)) xs
    }
