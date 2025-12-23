-- |
-- Module      : AOC2022.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day18 (
  day18a,
  day18b,
)
where

import AOC.Common (countTrue, floodFill, listV3, perturbations, readAll)
import AOC.Common.Point (boundingBox')
import AOC.Solver ((:~>) (..))
import Control.Monad ((<=<))
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Linear (V2 (..), V3 (..))

cardinalNeighbs3 :: V3 Int -> [V3 Int]
cardinalNeighbs3 = perturbations \i -> [i - 1, i + 1]

parseLine :: String -> Maybe (V3 Int)
parseLine = readAll <=< listV3 . splitOn ","

day18a :: [V3 Int] :~> Int
day18a =
  MkSol
    { sParse = traverse parseLine . lines
    , sShow = show
    , sSolve = \pts ->
        Just
          let ptsSet = S.fromList pts
           in countTrue (`S.notMember` ptsSet) $ concatMap cardinalNeighbs3 pts
    }

day18b :: [V3 Int] :~> Int
day18b =
  MkSol
    { sParse = traverse parseLine . lines
    , sShow = show
    , sSolve = \pts ->
        boundingBox' pts <&> \(V2 mins maxs) ->
          let ptsSet = S.fromList pts
              outerRegions = flip floodFill (S.fromList [mins - 1, maxs + 1]) \p ->
                S.fromList
                  [ q
                  | q <- cardinalNeighbs3 p
                  , and $ (>=) <$> q <*> (mins - 1)
                  , and $ (<=) <$> q <*> (maxs + 1)
                  , q `S.notMember` ptsSet
                  ]
           in countTrue (`S.member` outerRegions) $ concatMap cardinalNeighbs3 pts
    }
