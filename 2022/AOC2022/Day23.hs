-- |
-- Module      : AOC2022.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day23 (
  day23a,
  day23b,
)
where

import AOC.Common (freqs, slidingPairs)
import AOC.Common.Point (
  Dir (..),
  Point,
  boundingBox,
  dirPoint,
  fullNeighbsSet,
  parseAsciiSet,
  rotPoint,
 )
import AOC.Solver ((:~>) (..))
import Control.Monad (guard)
import Data.Foldable (asum, toList)
import Data.List (findIndex, scanl')
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import Linear.V2 (V2 (..))

step :: Set Point -> Int -> Set Point
step xs n = S.fromList . toList $
  flip M.mapWithKey proposalMap $ \p0 x ->
    if allProps M.! x > 1
      then p0
      else x
  where
    proposalMap = M.fromSet makeProposal xs
    makeProposal p
      | allClear = p
      | otherwise =
          maybe 0 ((+ p) . dirPoint) . asum . shift $
            [ d <$ guard (S.null (neighbs `S.intersection` clearance))
            | d <- [South, North, West, East]
            , let clearance =
                    S.fromList $
                      (+ p) . rotPoint d <$> [V2 (-1) 1, V2 0 1, V2 1 1]
            ]
      where
        neighbs = fullNeighbsSet p `S.intersection` xs
        allClear = S.null neighbs
    allProps = freqs $ proposalMap
    shift = take 4 . drop (n `mod` 4) . cycle

countEmpty :: NESet Point -> Int
countEmpty xs = product (maxs - mins + 1) - NES.size xs
  where
    V2 mins maxs = boundingBox xs

day23a :: Set Point :~> NESet Point
day23a =
  MkSol
    { sParse = Just . parseAsciiSet (== '#')
    , sShow = show . countEmpty
    , sSolve = \xs -> NES.nonEmptySet $ foldl' step xs [0 .. 9]
    }

day23b :: Set Point :~> Int
day23b =
  MkSol
    { sParse = Just . parseAsciiSet (== '#')
    , sShow = show
    , sSolve = \xs ->
        fmap (+ 1)
          . findIndex (uncurry (==))
          . slidingPairs
          . scanl' step xs
          $ [0 ..]
    }
