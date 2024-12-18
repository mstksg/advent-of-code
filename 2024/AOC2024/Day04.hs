-- |
-- Module      : AOC2024.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day04 (
  day04a,
  day04b,
)
where

import AOC.Common (countTrue, mapFromStore, mapToStore, matchMap)
import AOC.Common.Point (Point, fullNeighbs, parseAsciiMap, (*^))
import AOC.Solver (noFail, type (:~>) (..))
import Control.Comonad.Store (Comonad (extend), Store)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2 (V2 (..))

xmas :: Set (Map Point Char)
xmas =
  S.fromList
    [ M.fromList [(i *^ d, x) | (i, x) <- zip [0 ..] "XMAS"]
    | d <- fullNeighbs 0
    ]

crossMas :: Set (Map Point Char)
crossMas =
  S.fromList
    [ M.insert 0 'A' (diag1 <> diag2)
    | diag1 <- M.fromList . zip [V2 (-1) (-1), V2 1 1] <$> ["MS", "SM"]
    , diag2 <- M.fromList . zip [V2 1 (-1), V2 (-1) 1] <$> ["MS", "SM"]
    ]

matchAnyMap :: (Num k, Eq a, Foldable f) => f (Map k a) -> Store k (Maybe a) -> Int
matchAnyMap mps x = countTrue (`matchMap` x) mps

day04 :: Set (Map Point Char) -> Map Point Char :~> Int
day04 stencils =
  MkSol
    { sParse = noFail $ parseAsciiMap Just
    , sShow = show
    , sSolve = noFail \xs ->
        sum . mapFromStore (M.keysSet xs) . extend (matchAnyMap stencils) $ mapToStore xs
    }

day04a :: Map Point Char :~> Int
day04a = day04 xmas

day04b :: Map Point Char :~> Int
day04b = day04 crossMas
