-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
module AOC2023.Day07 (
  day07a,
  day07b,
)
where

import AOC.Common (freqs)
import AOC.Common.Parser (CharParser, pDecimal, parseLines, tokenMap)
import AOC.Solver (noFail, (:~>) (..))
import Control.Monad (replicateM)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (Down (..))

data Card
  = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving stock (Eq, Ord, Enum, Show)

part1Cards :: Map Char Card
part1Cards =
  M.fromList $
    zip "23456789TJQKA" [Two .. Ace]

part2Cards :: Map Char Card
part2Cards = M.insert 'J' Joker part1Cards

handType :: [Card] -> [Int]
handType cs = maybe id addJoker numJokers $ sortOn Down (M.elems fs')
  where
    (numJokers, fs') =
      M.alterF (,Nothing) Joker $
        freqs cs
    addJoker n = \case
      [] -> [n]
      x : xs -> (x + n) : xs

parseLine :: Map Char Card -> CharParser ([Card], Int)
parseLine mp =
  (,)
    <$> replicateM 5 (tokenMap mp)
    <*> pDecimal

day07 :: Map Char Card -> [([Card], Int)] :~> Int
day07 parseCard =
  MkSol
    { sParse = parseLines $ parseLine parseCard
    , sShow = show
    , sSolve =
        noFail $
          sum
            . zipWith (\i (_, b) -> i * b) [1 ..]
            . sortOn (\(q, _) -> (handType q, q))
    }

day07a :: [([Card], Int)] :~> Int
day07a = day07 part1Cards

day07b :: [([Card], Int)] :~> Int
day07b = day07 part2Cards
