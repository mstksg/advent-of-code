{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2025.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
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
module AOC2025.Day08 (
  day08a,
  day08b,
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

buildGraph :: [V3 Int] -> Int -> ((V3 Int, V3 Int), G.Gr (V3 Int) Double)
buildGraph pts = \n ->
  ( let (_, ((_, p), (_, q))) = dists !! (n - 1)
     in (p, q)
  , G.mkGraph
      nodepts
      [(i, j, sqrt $ fromIntegral d) | (d, ((i, _), (j, _))) <- take n dists]
  )
  where
    nodepts :: [(Int, V3 Int)]
    nodepts = zip [0 ..] pts
    dists = sort [(p `L.qd` q, (pp, qq)) | pp@(_, p) : ps <- tails nodepts, qq@(_, q) <- ps]

day08a :: [V3 Int] :~> Int
day08a =
  MkSol
    { sParse =
        traverse (listV3 <=< traverse readMaybe . splitOn ",") . lines
    , sShow = show
    , sSolve =
        noFail \pts ->
          let ixedPts = zip [0 ..] pts
              dists =
                take (dyno_ "pairs" 1000) $ sort [(p `L.qd` q, (i, j)) | (i, p) : ps <- tails ixedPts, (j, q) <- ps]
           in product . take 3 . sortOn Down . map length . G.components $
                G.mkUGraph @G.Gr (fst <$> ixedPts) (snd <$> dists)
    }

day08b :: [V3 Int] :~> _
day08b =
  MkSol
    { sParse = sParse day08a
    , sShow = show
    , sSolve =
        \pts -> do
          let ixedPts = zip [0 ..] pts
              gr =
                G.mkGraph @G.Gr
                  ixedPts
                  [(i', j', p `L.qd` q) | (i, p) : ps <- tails ixedPts, (j, q) <- ps, (i', j') <- [(i, j), (j, i)]]
              mst = G.msTree gr
          (_, (i, j)) <-
            maximumMay $
              [ (w, (a, b))
              | G.LP path <- mst
              , ((a, w), (b, _)) <- slidingPairs path
              ]
          V3 px _ _ <- G.lab gr i
          V3 qx _ _ <- G.lab gr j
          pure $ px * qx
    }

-- 405, 1548 ... spent 15 minutes debugging inits vs tails lol
