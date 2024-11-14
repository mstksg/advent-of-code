-- |
-- Module      : AOC2022.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day12 (
  day12a,
  day12b,
)
where

import AOC.Common (charFinite)
import AOC.Common.Point (Point, cardinalNeighbsSet, mannDist, parseAsciiMap)
import AOC.Common.Search (aStar)
import AOC.Solver ((:~>) (..))
import Data.Finite (Finite, shift, strengthen)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

data Tile = Start | Terrain (Finite 26) | End
  deriving stock (Show, Eq, Ord)

parseChar :: Char -> Maybe Tile
parseChar 'S' = Just Start
parseChar 'E' = Just End
parseChar c = Terrain . snd <$> charFinite c

day12a :: Map Point Tile :~> Int
day12a =
  MkSol
    { sParse = Just . parseAsciiMap parseChar
    , sShow = show
    , sSolve = \heightMap -> do
        let reverseMap =
              M.fromListWith
                (<>)
                [ (t, S.singleton p)
                | (p, t) <- M.toList heightMap
                ]
        sPos <- S.lookupMin =<< M.lookup Start reverseMap
        ePos <- S.lookupMin =<< M.lookup End reverseMap
        let expand p = neighbs `S.intersection` limiter
              where
                neighbs = cardinalNeighbsSet p
                limiter = fold . M.restrictKeys reverseMap . S.fromList $ case heightMap M.! p of
                  Start -> [Start, Terrain 0]
                  Terrain i ->
                    Start
                      : maybe End Terrain (strengthen (shift i))
                      : (Terrain <$> [0 .. i])
                  End -> []
        fst
          <$> aStar
            (mannDist ePos)
            (M.fromSet (const 1) . expand)
            sPos
            (== ePos)
    }

day12b :: Map Point Tile :~> Int
day12b =
  MkSol
    { sParse = Just . parseAsciiMap parseChar
    , sShow = show
    , sSolve = \heightMap -> do
        let reverseMap =
              M.fromListWith
                (<>)
                [ (t, S.singleton p)
                | (p, t) <- M.toList heightMap
                ]
        ePos <- S.lookupMin =<< M.lookup End reverseMap
        let expand Nothing = fold $ M.restrictKeys reverseMap (S.fromList [Start, Terrain 0])
            expand (Just p) = neighbs `S.intersection` limiter
              where
                neighbs = cardinalNeighbsSet p
                limiter = fold . M.restrictKeys reverseMap . S.fromList $ case heightMap M.! p of
                  Start -> [Start, Terrain 0]
                  Terrain i ->
                    Start
                      : maybe End Terrain (strengthen (shift i))
                      : (Terrain <$> [0 .. i])
                  End -> []
        subtract 1 . fst
          <$> aStar
            (maybe maxBound (mannDist ePos))
            (M.fromSet (const 1) . S.map Just . expand)
            Nothing
            (== Just ePos)
    }
