{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day02
  ( 
    day02a,
    day02b
  )
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntMap.NonEmpty as IM
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

day02a :: _ :~> _
day02a =
  MkSol
    { sParse = parseLines (P.many pDecimal)
    ,
      sShow = show,
      sSolve =
        noFail $
          countTrue \xs -> 
            let diffies = zipWith subtract xs (drop 1 xs)
             in all (\x -> x `elem` [1,2,3]) (abs <$> diffies)
                  && (all (> 0) diffies || all (< 0) diffies)
    }

day02b :: _ :~> _
day02b =
  MkSol
    { sParse = sParse day02a,
      sShow = show,
      sSolve =
        noFail $
          countTrue \xs -> 
            let possibilities = xs : zipWith (++) (inits xs) (drop 1 $ tails xs)
             in any pred possibilities

    }
  where
    pred xs = 
            let diffies = zipWith subtract xs (drop 1 xs)
             in all (\x -> x `elem` [1,2,3]) (abs <$> diffies)
                  && (all (> 0) diffies || all (< 0) diffies)
