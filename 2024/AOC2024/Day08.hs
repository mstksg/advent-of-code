-- |
-- Module      : AOC2024.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day08 (
  day08a,
  day08b,
)
where

import AOC.Common.Point (Point, parseAsciiMap)
import AOC.Solver (noFail, type (:~>) (..))
import Control.Monad (guard, mfilter)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

makeAntinodes :: Eq a => Map Point a -> (Point -> Point -> [Point]) -> Set Point
makeAntinodes mp genPts = S.fromList do
  (p1, c1) <- M.toList mp
  (p2, c2) <- M.toList mp
  guard $ p1 /= p2 && c1 == c2
  genPts p1 p2

day08 :: (Point -> Point -> [Point]) -> Map Point (Maybe Char) :~> Int
day08 stepper =
  MkSol
    { sParse = Just . parseAsciiMap (Just . mfilter (/= '.') . Just)
    , sShow = show
    , sSolve = noFail \mp ->
        let allPoints = M.keysSet mp
            ants = M.mapMaybe id mp
         in S.size $ makeAntinodes ants \p1 p2 ->
              takeWhile (`S.member` allPoints) $ stepper p1 p2
    }

day08a :: Map Point (Maybe Char) :~> Int
day08a = day08 \p1 p2 -> [p2 + p2 - p1]

day08b :: Map Point (Maybe Char) :~> Int
day08b = day08 \p1 p2 -> iterate (+ (p2 - p1)) p2
