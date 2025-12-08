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
  day08b
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

day08a :: [V3 Int] :~> _
day08a =
  MkSol
    { sParse =
        noFail $
          map ((\[a,b,c] -> V3 a b c) . map read . splitOn ",") . lines
    , sShow = show
    , sSolve =
        noFail \pts ->
          let dists = sortOn fst $
                  [ (sqrt . fromIntegral . sum $ fmap (^2) (p - q), (p, q))
                    | p:ps <- tails pts
                    , q <- ps
                  ]
              nodepts = zip [0..] pts
              nodemap :: Map (V3 Int) Int
              nodemap = M.fromList $ map swap nodepts
              gr = G.mkUGraph @G.Gr
                  (fst <$> nodepts)
                  [ (nodemap M.! p, nodemap M.! q) | pq <- snd <$> take 1000 dists, (p,q) <- [pq, swap pq]]
          in product . take 3 . sortOn negate . map length $ G.components gr
    }

              -- conns = snd <$> take 10 dists
              -- grab :: Ord a => (Set (Set a), [(a, a)]) -> (Bool, (Set (Set a), [(a,a)]))
              -- grab (clusts, (p,q):pqs) = do 
              --   let Just pclust = find (p `S.member`) clusts
              --       Just qclust = find (q `S.member`) clusts
              --       clusts' = S.insert (pclust <> qclust) . S.delete pclust . S.delete qclust $ clusts
              --    in (clusts' /= clusts, (clusts', pqs))
              -- groupUpN n
              --   | n == 0 = pure ()
              --   | otherwise = do
              --       found <- state grab
              --       if found
              --          then groupUpN (n - 1)
              --          else groupUpN n
            -- -- in take 5 $ snd <$> dists
          -- -- in execState (groupUpN 2) (S.fromList $ map S.singleton pts, snd <$> dists)
          -- -- in product . take 3
          -- --       . sortBy (flip compare) . map S.size . S.toList . fst $ execState (groupUpN 11)
          -- --         (S.fromList $ map S.singleton pts, snd <$> dists)

day08b :: _ :~> _
day08b =
  MkSol
    { sParse = sParse day08a
    , sShow = show
    , sSolve =
        \pts -> do
          let dists = sortOn fst $
                  [ (sqrt . fromIntegral . sum $ fmap (^2) (p - q), (p, q))
                    | p:ps <- tails pts
                    , q <- ps
                  ]
              nodepts = zip [0..] pts
              nodemap :: Map (V3 Int) Int
              nodemap = M.fromList $ map swap nodepts
              gr n = G.mkUGraph @G.Gr
                  (fst <$> nodepts)
                  [ (nodemap M.! p, nodemap M.! q) | pq <- snd <$> take n dists, (p,q) <- [pq, swap pq]]
          neededDists <- binaryMinSearch (G.isConnected . gr) 0 (length dists)
          let (p,q) = snd $ dists  !! (neededDists - 1)
          pure $ view _x p * view _x q
-- binaryMinSearch ::
--   (Int -> Bool) ->
--   Int ->
--   Int ->
--   Maybe Int
    }
