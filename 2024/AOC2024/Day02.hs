-- |
-- Module      : AOC2024.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day02 (
  day02a,
  day02b,
)
where

import AOC.Common (countTrue)
import AOC.Common.Parser (pDecimal, parseLines)
import AOC.Solver (noFail, (:~>) (..))
import Control.Applicative (many)
import Data.Ix (inRange)
import Data.List (inits, tails)

predicate :: [Int] -> Bool
predicate xs = any (all (inRange (1, 3))) [diffies, negate <$> diffies]
  where
    diffies = zipWith subtract xs (drop 1 xs)

day02a :: [[Int]] :~> Int
day02a =
  MkSol
    { sParse = parseLines (many pDecimal)
    , sShow = show
    , sSolve = noFail $ countTrue predicate
    }

day02b :: [[Int]] :~> Int
day02b =
  MkSol
    { sParse = sParse day02a
    , sShow = show
    , sSolve =
        noFail $
          countTrue \xs ->
            let possibilities = xs : zipWith (++) (inits xs) (drop 1 $ tails xs)
             in any predicate possibilities
    }
