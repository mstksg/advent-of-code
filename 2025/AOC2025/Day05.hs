{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2025.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
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
module AOC2025.Day05 (
  day05a,
  day05b,
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
import Data.ExtendedReal (Extended(..))
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP
import qualified Data.Interval as I
import qualified Data.IntervalSet as IVS

day05a :: _ :~> _
day05a =
  MkSol
    { sParse =
        noFail \xs -> case splitOn "\n\n" xs of
          [a,b] -> (fromJust . listTup . map (read @Int) . splitOn "-" <$> lines a, read @Int <$> lines b)
    , sShow = show
    , sSolve =
        noFail \(ranges, xs) ->
          countTrue (\x -> any (`inRange` x) ranges) xs
          -- let allRange = foldMap (IS.fromRange) ranges
          --  in IS.size $ IS.fromList xs `IS.intersection` allRange
    }

day05b :: _ :~> _
day05b =
  MkSol
    { sParse = sParse day05a
    , sShow = show
    , sSolve =
        noFail \(ranges, _) -> sum . map (succ . I.width) . IVS.toAscList $ foldMap (\(x,y) -> IVS.singleton $ Finite x I.<=..<= Finite y) ranges
          -- countTrue (\x -> any (`inRange` x) ranges) xs
    }

-- 402
