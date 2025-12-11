-- |
-- Module      : AOC2025.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day11 (
  day11a,
  day11b,
)
where

import AOC.Solver (noFail, (:~>) (..))
import Control.Monad ((<=<))
import Data.Bitraversable (bitraverse)
import Data.Functor ((<&>))
import Data.List (uncons)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Safe (initMay)

day11a :: [(String, [String])] :~> Int
day11a =
  MkSol
    { sParse =
        traverse (bitraverse initMay pure <=< uncons . words) . lines
    , sShow = show
    , sSolve = noFail \xs -> pathsTo (M.fromList xs) "you" "out"
    }

day11b :: [(String, [String])] :~> Int
day11b =
  MkSol
    { sParse = sParse day11a
    , sShow = show
    , sSolve =
        noFail \xs ->
          let toVisit = S.fromList ["dac", "fft"]
           in pathsTo (M.fromList xs `addVisited` toVisit) ("svr", toVisit) ("out", S.empty)
    }

pathsTo :: Ord a => Map a [a] -> a -> a -> Int
pathsTo conns start end = res M.! start
  where
    res =
      conns <&> \nexts ->
        sum
          [ if y == end then 1 else M.findWithDefault 0 y res
          | y <- nexts
          ]

addVisited :: Ord a => Map a [a] -> Set a -> Map (a, Set a) [(a, Set a)]
addVisited conns required =
  M.fromDistinctAscList
    [ ((x, subset), map (,S.delete x subset) xs)
    | (x, xs) <- M.toList conns
    , subset <- S.toList $ S.powerSet required
    ]
