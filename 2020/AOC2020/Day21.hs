{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC2020.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!
module AOC2020.Day21 (
  day21a,
  day21b,
)
where

import AOC.Common (countTrue, pickUnique)
import AOC.Common.Parser (parseLines)
import AOC.Solver ((:~>) (..))
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

assembleOptions ::
  (Ord k, Ord a) =>
  [(Set a, Set k)] ->
  [(k, Set a)]
assembleOptions info =
  M.toList . M.unionsWith S.intersection $
    info <&> \(igr, alg) -> M.fromSet (const igr) alg

day21a :: [(Set String, Set String)] :~> Int
day21a =
  MkSol
    { sParse = parseLines lineParser
    , sShow = show
    , sSolve = \igrsAlgs ->
        fmap (countNotIn (concatMap (toList . fst) igrsAlgs))
          . listToMaybe
          . map (S.fromList . toList)
          . pickUnique
          $ assembleOptions igrsAlgs
    }
  where
    countNotIn xs bad = countTrue (`S.notMember` bad) xs

day21b :: [(Set String, Set String)] :~> [String]
day21b =
  MkSol
    { sParse = parseLines lineParser
    , sShow = intercalate ","
    , sSolve = fmap toList . listToMaybe . pickUnique . assembleOptions
    }

type Parser = P.Parsec Void String

lineParser :: Parser (Set String, Set String)
lineParser =
  (,)
    <$> (S.fromList <$> P.many (P.some P.letterChar <* " "))
    <*> ( S.fromList
            <$> P.between
              "("
              ")"
              ("contains " *> P.some P.letterChar `P.sepBy` ", ")
        )
