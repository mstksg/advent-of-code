{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.
module AOC2024.Day10 (
  day10a,
  day10b,
)
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntMap.NonEmpty as NEIM
import qualified Data.IntSet as IS
import qualified Data.IntSet.NonEmpty as NEIS
import qualified Data.List.NonEmpty as NE
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map as M
import qualified Data.Map.NonEmpty as NEM
import qualified Data.OrdPSQ as PSQ
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

stepPoint :: (Enum a, Eq a) => Map Point a -> Point -> Set Point
stepPoint mp p = M.keysSet . M.filter (== succ x) $ mp `M.restrictKeys` cardinalNeighbsSet p
  where
    x = mp M.! p

distinctTrailsFrom :: (Eq a, Enum a, Bounded a) => Map Point a -> Point -> Int
distinctTrailsFrom mp = go
  where
    go p
      | x == maxBound = 1
      | otherwise = sum $ go <$> nexts
      where
        x = mp M.! p
        nexts = M.keys $ M.filter (== succ x) $ mp `M.restrictKeys` cardinalNeighbsSet p

day10a :: Map Point Int :~> Int
day10a =
  MkSol
    { sParse = noFail $ parseAsciiMap digitToIntSafe
    , sShow = show
    , sSolve =
        noFail \mp ->
          sum
            . map (\p -> M.size . M.filter (== 9) $ mp `M.restrictKeys` floodFill (stepPoint mp) (S.singleton p))
            . M.keys
            $ M.filter (== 0) mp
    }

day10b :: _ :~> _
day10b =
  MkSol
    { sParse = noFail $ parseAsciiMap (preview decimalDigit)
    , sShow = show
    , sSolve = noFail \mp -> sum . map (distinctTrailsFrom mp) . M.keys $ M.filter (== 0) mp
    }
