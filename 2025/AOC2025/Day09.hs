{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2025.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
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
module AOC2025.Day09 (
day09a,
day09b

)
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import Data.List (partition)
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

day09a :: _ :~> _
day09a =
  MkSol
    { sParse =
        noFail $
          mapMaybe (listV2 . map (read @Int) . splitOn ",") . lines
    , sShow = show
    , sSolve =
        noFail $
          \pts -> maximum
              [ (x + 1) * (y + 1)
                | p:ps <- tails pts
              , q <- ps
              , let V2 x y = abs $ p - q
              ]
              
    }

lineTo' x y = S.fromList (lineTo x y) -- <> S.fromList [x,y]

day09b :: _ :~> _
day09b =
  MkSol
    { sParse = sParse day09a
    -- , sShow = ('\n':) . displayAsciiSet '.' '#'
    , sShow = show
    , sSolve = \pts -> do
          let loopPts = zip pts (tail pts ++ [head pts])
              border = foldMap (uncurry lineTo') loopPts
              Just bb@(V2 bmin bmax) = boundingBox' pts
              loopMe turn = fold <$> go loopPts
                where
                  go ((p,q):xs) = (++) <$> newPoints <*> go xs
                    where
                      turnedMotion = motion <> turn
                      motion = case signum (q - p) of
                       V2 0 1 -> North
                       V2 1 0 -> East
                       V2 0 (-1) -> South
                       V2 (-1) 0 -> West
                       _ -> undefined
                      newPoints = for (lineTo p q) \r ->
                        let toEdge = traceShowId . takeWhile (inBoundingBox bb) . drop 1 $ iterate (+ dirPoint turnedMotion) r
                            (toLine, rest) = span (`S.notMember` border) toEdge
                         in S.fromList toLine <$ guard (not (null rest))
                  go [] = Just []
          interior <- loopMe East <> loopMe West
          let allPoints = interior <> border <> S.fromList pts
          maximumMay
              [ S.size rect
              | p:ps <- tails pts
              , q <- ps
              , let rect = traceShow (p,q) $ fillBoundingBox (V2 (min <$> p <*> q) (max <$> p <*> q))
              , S.null $ rect `S.difference` allPoints
              ]
              -- filled = fillBoundingBox bb
          -- let (verts, horiz) = partition (\(p, q) -> view _x p == view _x q) $ zipWith (,) pts (tail pts ++ [head pts])
          --     Just bb@(V2 bmin bmax) = boundingBox' pts
          --     filled = fillBoundingBox bb
          --     vertLines = foldMap (uncurry lineTo') verts
          --     horizLines = foldMap (uncurry lineTo') horiz
          --     inside = S.union (vertLines <> horizLines) $ flip S.filter filled \(V2 x y) ->
          --       odd (S.size (S.filter (\(V2 x' y') -> y' < y && x' == x) horizLines))
          --       && odd (S.size (S.filter (\(V2 x' y') -> x' < x && y' == y) vertLines))
          --  in inside
          -- let ptLoop = foldMap S.fromList $ zipWith lineTo pts (tail pts ++ [head pts])
          --     Just bb@(V2 bmin bmax) = boundingBox' pts
          --     filled = fillBoundingBox bb
          --     cand1 = floodFill ((`S.difference` ptLoop) . (`S.intersection` filled) . cardinalNeighbsSet)  (S.fromList $ V2 <$> toList bmin <*> toList bmax)
          --     cand2 = (filled `S.difference` cand1) <> ptLoop
-- -- lineTo :: Point -> Point -> [Point]
          --  in cand2

-- -- | Flood fill from a starting set
-- floodFill ::
--   Ord a =>
--   -- | Expansion (be sure to limit allowed points)
--   (a -> Set a) ->
--   -- | Start points
--   Set a ->
--   -- | Flood filled
--   Set a
-- floodFill f = snd . floodFillCount f

            
            -- let (map snd->rs, map snd->gs) = partition (even . fst) $ zip [0..] pts
            --     rPairs = (zip rs (tail rs ++ [head rs])) <&> \(V2 x y, V2 x' y') ->
            --               fillBoundingBox (V2 (V2 (min x x') (min y y')) (V2 (max x x') (max y y')))
            --     gPairs = (zip gs (tail gs ++ [head gs])) <&> \(V2 x y, V2 x' y') ->
            --               fillBoundingBox (V2 (V2 (min x x') (min y y')) (V2 (max x x') (max y y')))
            --  in maximum
            --         [ S.size (xs <> ys)
            --           | xs <- rPairs
            --           , ys <- gPairs
            --           ]
    }
