-- |
-- Module      : AOC2024.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day03 (
  day03a,
  day03b,
)
where

import AOC.Common.Parser (CharParser, pDropUntil, parseMaybe')
import AOC.Solver (type (:~>) (..))
import Control.Applicative (Alternative (many))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as PL

parseMul :: CharParser Int
parseMul = product <$> P.between "mul(" ")" (PL.decimal `P.sepBy` ",")

day03a :: String :~> Int
day03a =
  MkSol
    { sParse = Just
    , sShow = show
    , sSolve = parseMaybe' $ sum <$> many (pDropUntil parseMul)
    }

doOrDoNot :: CharParser Int
doOrDoNot = sum <$> goDisabled
  where
    goDisabled :: CharParser [Int]
    goDisabled = P.option [] . pDropUntil $ "do()" *> goEnabled
    goEnabled :: CharParser [Int]
    goEnabled =
      P.option [] . pDropUntil . P.choice $
        [ "don't()" *> goDisabled
        , (:) <$> parseMul <*> goEnabled
        ]

day03b :: String :~> Int
day03b =
  MkSol
    { sParse = Just
    , sShow = show
    , sSolve = parseMaybe' doOrDoNot
    }
