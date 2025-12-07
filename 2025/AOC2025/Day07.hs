{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2025.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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
module AOC2025.Day07 (
  day07a,
  day07b,
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

parseMap :: String -> Maybe (Point, NESet Point)
parseMap =
  reshape
    . M.partition id
    . parseAsciiMap \case 'S' -> Just True; '^' -> Just False; _ -> Nothing
  where
    reshape (startPos, rest) = (,) . fst <$> M.lookupMin startPos <*> NES.nonEmptySet (M.keysSet rest)

day07a :: _ :~> _
day07a =
  MkSol
    { sParse = parseMap
    , sShow = show
    , sSolve =
        noFail $ \(startPos, splitters) ->
          let pathsTo :: NEMap Point Bool
              pathsTo = flip NEM.fromSet splitters \(V2 x y0) ->
                let cands = takeWhile ((`NES.notMember` splitters) . V2 x) [y0 - 1, y0 - 2 .. 0]
                 in flip any cands \y ->
                      V2 x y == startPos
                        || NEM.findWithDefault False (V2 (x - 1) y) pathsTo
                        || NEM.findWithDefault False (V2 (x + 1) y) pathsTo
           in countTrue id pathsTo
    }

day07b :: _ :~> _
day07b =
  MkSol
    { sParse = parseMap
    , sShow = show
    , sSolve = \(startPos, splitters) -> do
        let maxY = maximum $ NES.map (view _y) splitters
            downFrom (V2 x y0) = fromMaybe 1 $ flip firstJust [y0 .. maxY] \y ->
              NEM.lookup (V2 x y) pathsFrom
            pathsFrom :: NEMap Point Int
            pathsFrom = flip NEM.fromSet splitters \p ->
              downFrom (p + V2 1 2) + downFrom (p + V2 (-1) 2)
        NEM.lookup (startPos + V2 0 2) pathsFrom
    }

-- 502, 1491
