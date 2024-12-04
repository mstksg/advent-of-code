{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day04 (
  day04a,
  day04b,
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

day04a :: _ :~> _
day04a =
  MkSol
    { sParse =
        noFail $
          lines
    , sShow = show
    , sSolve = noFail \xs ->
        let findxms = countTrue p . tails
              where
                p x = "XMAS" `isPrefixOf` x || "SAMX" `isPrefixOf` x
         in sum . map (sum . map findxms) $
              [ xs
              , transpose xs
              , transpose . zipWith drop [0 ..] $ xs
              , drop 1 . transpose . zipWith drop [0 ..] . reverse . map reverse $ xs
              , transpose . zipWith drop [0 ..] . reverse $ xs
              , drop 1 . transpose . zipWith drop [0 ..] . map reverse $ xs
              ]
    }

day04b :: _ :~> _
day04b =
  MkSol
    { sParse = noFail $ parseAsciiMap Just
    , sShow = show
    , sSolve = noFail \mp -> flip countTrue (M.toList mp) \(p, x) ->
        case x of
          'A' ->
            let nw = mfilter (`elem` asString "SM") $ M.lookup (p - V2 1 1) mp
                ne = mfilter (`elem` asString "SM") $ M.lookup (p + V2 1 (-1)) mp
                sw = mfilter (`elem` asString "SM") $ M.lookup (p + V2 (-1) 1) mp
                se = mfilter (`elem` asString "SM") $ M.lookup (p + V2 1 1) mp
                xmas1 = (/=) <$> nw <*> se
                xmas2 = (/=) <$> ne <*> sw
                xmas = (&&) <$> xmas1 <*> xmas2
            in  fromMaybe False xmas
          _   -> False
    }
