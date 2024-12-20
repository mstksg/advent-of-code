{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day20 (
  day20a,
  day20b,
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

day20a :: _ :~> _
day20a =
  MkSol
    { sParse =
        noFail $ parseAsciiMap \case
          'S' -> Just $ Just False
          'E' -> Just $ Just True
          '#' -> Just Nothing
          _ -> Nothing
    , sShow = show
    , sSolve = \mp -> do
        start : _ <- pure . M.keys $ M.filter (== Just False) mp
        end : _ <- pure . M.keys $ M.filter (== Just True) mp
        bb <- boundingBox' $ M.keysSet mp
        let walls = M.keysSet $ M.filter isNothing mp
            cheats =
              S.fromList
                [ (w, d)
                | w <- toList walls
                , dir <- [ North ..]
                , let d = w + dirPoint dir
                      d' = w - dirPoint dir
                , d `S.notMember` walls
                , d' `S.notMember` walls
                -- toList $ cardinalNeighbsSet w `S.difference` walls
                , inBoundingBox bb d
                , inBoundingBox bb d'
                ]
            cheatPaths = do
              (w, d) <- toList cheats
              traceM $ show (w, d)
              let go p
                    | p == w = S.singleton d
                    | otherwise = cardinalNeighbsSet p `S.difference` S.delete d walls
              maybeToList $ fst <$> aStar (mannDist end) (M.fromSet (const 1) . go) start (== end)
        goodPath <-
          let go p = cardinalNeighbsSet p `S.difference` walls
           in length <$> bfs go start (== end)
        traceM $ show goodPath
        pure $ countTrue (\t -> t - goodPath >= 100) cheatPaths
    }

-- aStar ::
--   forall n p.
--   (Ord n, Ord p, Num p) =>
--   -- | heuristic
--   (n -> p) ->
--   -- | neighborhood
--   (n -> Map n p) ->
--   -- | start
--   n ->
--   -- | target
--   (n -> Bool) ->
--   -- | the shortest path, if it exists, and its cost
--   Maybe (p, [n])

day20b :: _ :~> _
day20b =
  MkSol
    { sParse = sParse day20a
    , sShow = show
    , sSolve =
        noFail $
          id
    }
