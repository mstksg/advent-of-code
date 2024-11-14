-- |
-- Module      : AOC2022.Day25
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day25 (
  day25a,
  decToSnafu,
)
where

import AOC.Solver ((:~>) (..))
import Control.DeepSeq (NFData)
import Data.List (unfoldr)
import GHC.Generics (Generic)

data SnafuDigit = DubMin | Minus | Zero | One | Two
  deriving stock (Show, Eq, Ord, Generic)

instance NFData SnafuDigit

parseSnafuDigit :: Char -> Maybe SnafuDigit
parseSnafuDigit = \case
  '=' -> Just DubMin
  '-' -> Just Minus
  '0' -> Just Zero
  '1' -> Just One
  '2' -> Just Two
  _ -> Nothing

showSnafuDigit :: SnafuDigit -> Char
showSnafuDigit = \case
  DubMin -> '='
  Minus -> '-'
  Zero -> '0'
  One -> '1'
  Two -> '2'

snafuToDec :: [SnafuDigit] -> Int
snafuToDec = sum . map go . zip (iterate (* 5) 1) . reverse
  where
    go (j, c) = case c of
      DubMin -> -2 * j
      Minus -> -j
      Zero -> 0
      One -> j
      Two -> 2 * j

decToSnafu :: Int -> [SnafuDigit]
decToSnafu = reverse . unfoldr go
  where
    go i = case i `divMod` 5 of
      (0, j) | j <= 0 -> Nothing
      (j, 0) -> Just (Zero, j)
      (j, 1) -> Just (One, j)
      (j, 2) -> Just (Two, j)
      (j, 3) -> Just (DubMin, j + 1)
      (j, 4) -> Just (Minus, j + 1)
      _ -> Nothing

day25a :: [[SnafuDigit]] :~> [SnafuDigit]
day25a =
  MkSol
    { sParse = (traverse . traverse) parseSnafuDigit . lines
    , sShow = map showSnafuDigit
    , sSolve = Just . decToSnafu . sum . map snafuToDec
    }
