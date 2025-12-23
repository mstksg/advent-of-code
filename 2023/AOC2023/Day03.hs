-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
module AOC2023.Day03 (
  day03a,
  day03b,
)
where

import AOC.Common (listTup, readAll)
import AOC.Common.Point (Point, contiguousRegions, fullNeighbs, fullNeighbsSet, parseAsciiMap)
import AOC.Solver ((:~>) (..))
import Control.Lens (each, traverseOf)
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import Text.Read (readMaybe)

day03a :: (Set Point, Map Point Char) :~> Int
day03a =
  MkSol
    { sParse =
        Just
          . first M.keysSet
          . M.mapEither id
          . parseAsciiMap
            ( \case
                '.' -> Nothing
                c
                  | isDigit c -> Just $ Right c
                  | otherwise -> Just $ Left ()
            )
    , sShow = show
    , sSolve = \(symbolPoints, numPoints) ->
        let numChunks = contiguousRegions (M.keysSet numPoints)
            adjacentToSymbols = fullNeighbsSet `foldMap` S.toList symbolPoints
            validNumChunks = flip S.filter numChunks \numChunk ->
              not $ NES.toSet numChunk `S.disjoint` adjacentToSymbols
            numStrings =
              M.elems
                . M.restrictKeys numPoints
                . NES.toSet
                <$> S.toList validNumChunks
         in sum <$> readAll numStrings
    }

day03b :: (Set Point, Map Point Char) :~> Int
day03b =
  MkSol
    { sParse =
        Just
          . first M.keysSet
          . M.mapEither id
          . parseAsciiMap
            ( \case
                '*' -> Just $ Left ()
                c -> Right c <$ guard (isDigit c)
            )
    , sShow = show
    , sSolve = \(symbolPoints, numPoints) ->
        let numChunks = contiguousRegions (M.keysSet numPoints)
            adjacentToSymbols = M.fromList do
              p <- S.toList symbolPoints
              n <- fullNeighbs p
              pure (n, p)
            validNumChunks = M.fromListWith (<>) do
              rg <- NES.toSet <$> S.toList numChunks
              let rgDigits = M.elems $ M.restrictKeys numPoints rg
              orig <-
                M.elems $
                  adjacentToSymbols `M.restrictKeys` rg
              pure (orig, S.singleton rgDigits)
         in fmap (sum . map (uncurry (*)))
              . traverse (traverseOf each readMaybe)
              . mapMaybe (listTup . S.elems)
              $ M.elems validNumChunks
    }
