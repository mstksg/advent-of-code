-- |
-- Module      : AOC2024.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day05 (
  day05a,
  day05b,
)
where

import AOC.Common (middleVal)
import AOC.Common.Parser (
  CharParser,
  pDecimal,
  parseMaybe',
  sepBy',
  sepByLines,
  sepEndByLines,
  sequenceSepBy,
 )
import AOC.Solver (noFail, type (:~>) (..))
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (Foldable (toList))
import qualified Data.Graph.Inductive as G
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Linear.V2 (V2 (..))
import qualified Text.Megaparsec.Char as P

parseInput :: CharParser ([V2 Int], [[Int]])
parseInput = do
  rules <- sepEndByLines $ V2 pDecimal pDecimal `sequenceSepBy` "|"
  P.newline
  pages <- sepByLines $ pDecimal `sepBy'` ","
  pure (rules, pages)

sortByRules :: [V2 Int] -> [Int] -> [Int]
sortByRules rules = \xs ->
  G.topsort . G.nfilter (`S.member` S.fromList xs) $ rulesGraph
  where
    rulesGraph :: G.Gr () ()
    rulesGraph =
      G.mkUGraph (nubOrd $ foldMap toList rules) [(x, y) | V2 x y <- rules]

day05a :: ([V2 Int], [[Int]]) :~> Int
day05a =
  MkSol
    { sParse = parseMaybe' parseInput
    , sShow = show
    , sSolve =
        noFail \(rules, pages) ->
          let sbr = sortByRules rules
           in sum
                [ fromMaybe 0 $ middleVal orig
                | orig <- pages
                , orig == sbr orig
                ]
    }

day05b :: ([V2 Int], [[Int]]) :~> Int
day05b =
  MkSol
    { sParse = parseMaybe' parseInput
    , sShow = show
    , sSolve =
        noFail \(rules, pages) ->
          let sbr = sortByRules rules
           in sum
                [ fromMaybe 0 $ middleVal sorted
                | orig <- pages
                , let sorted = sbr orig
                , orig /= sorted
                ]
    }
