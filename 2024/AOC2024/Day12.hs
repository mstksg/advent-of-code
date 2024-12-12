{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day12 (
day12a,
day12b

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

perimeter :: Set Point -> Int
perimeter pts = sum
  [ S.size $ cardinalNeighbsSet p `S.difference` pts
    | p <- toList pts
  ]

day12a :: _ :~> _
day12a =
  MkSol
    { sParse = noFail $ parseAsciiMap Just
    , sShow = show
    , sSolve =
        noFail \mp ->
          let regions = contiguousRegions <$> M.fromListWith (<>)
                  [ (x, S.singleton p)
                    | (p, x) <- M.toList mp
                  ]
          in sum [ perimeter reg * S.size reg | regs <- toList regions, reg <- NES.toSet <$> toList regs ]
    }

sides :: Set Point -> Int
sides pts = sum . map S.size . toList $ contiguousRegions <$> M.fromListWith (<>)
  [ (d, S.singleton p)
    | p <- toList pts
    , d <- [ North .. ]
  , (p + dirPoint d) `S.notMember` pts
  ]

day12b :: _ :~> _
day12b =
  MkSol
    { sParse = sParse day12a
    , sShow = show
    , sSolve =
        noFail \mp ->
          let regions = contiguousRegions <$> M.fromListWith (<>)
                  [ (x, S.singleton p)
                    | (p, x) <- M.toList mp
                  ]
          in sum [ sides reg * S.size reg | regs <- toList regions, reg <- NES.toSet <$> toList regs ]
    }
