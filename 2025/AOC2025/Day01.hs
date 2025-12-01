-- |
-- Module      : AOC2025.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day01 (
  day01a,
  day01b,
)
where

import AOC.Common (countTrue)
import AOC.Solver (noFail, type (:~>) (..))
import Data.Finite (modulo)
import Data.List (scanl')
import Data.Traversable (mapAccumL)
import Text.Read (readMaybe)

day01a :: [Integer] :~> Int
day01a =
  MkSol
    { sParse = traverse (readMaybe . map align) . lines
    , sShow = show
    , sSolve =
        noFail $
          countTrue (== 0) . scanl' (+) 50 . map (modulo @100)
    }
  where
    align = \case
      'R' -> ' '
      'L' -> '-'
      d -> d

day01b :: [Integer] :~> Integer
day01b =
  MkSol
    { sParse = sParse day01a
    , sShow = show
    , sSolve =
        noFail $ sum . snd . mapAccumL go 50
    }
  where
    go curr bump = (m, hits)
      where
        (d, m) = (curr + bump) `divMod` 100
        -- uh...it works ok
        hits
          | bump > 0 = d
          | m == 0 = abs d + 1
          | curr == 0 = abs d - 1
          | otherwise = abs d
