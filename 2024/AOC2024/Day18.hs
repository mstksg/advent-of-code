{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day18 (
day18a,
day18b
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

day18a :: _ :~> _
day18a =
  MkSol
    { sParse = parseMaybe' $ sepByLines $ sequenceSepBy  (V2 pDecimal pDecimal) ","
    , sShow = show
    , sSolve =
        \pts ->
          let walls = S.fromList $ take 1024 pts
              step p = S.filter (all (inRange (0, 70))) $ cardinalNeighbsSet p `S.difference` walls
           in length <$> bfs step 0 (== 70)
          
    }

day18b :: _ :~> _
day18b =
  MkSol
    { sParse = sParse day18a
    -- , sShow = ('\n':) . displayAsciiMap ' '
    -- , sShow = show
    , sShow = intercalate "," . map show . toList
    , sSolve = \pts -> do
      let step walls p = S.filter (all (inRange (0, 70))) $ cardinalNeighbsSet p `S.difference` walls
          testI i = do
            let walls = S.fromList $ take i pts
            bfs (step walls) 0 (== 70)
            -- aStar (const 0) (M.fromSet (const 1) . step walls) 0 (== 70)
      j <- binaryMinSearch (isNothing . testI) 0 (length pts)
      pure $ pts !! (j - 1)
      -- pure $ testI (j + 2)
      -- pure $ M.singleton (pts !! (j + 2)) '2'
      --     <> M.singleton (pts !! (j + 0)) '0'
      --     <> M.singleton (pts !! (j  -1)) '&'
      --     <> M.singleton (pts !! (j + 1)) '1'
      --     <> M.singleton (pts !! (j + 2)) '2'
      --     <> M.singleton (pts !! (j + 3)) '3'
      --     <> M.singleton (pts !! (j + 4)) '4'
      --     <> M.fromSet (const '#') (S.fromList $ take j pts)
      --     <> M.fromSet (const 'o') (foldMap S.fromList $ testI (j - 1))
      -- pure $ testI j

-- binaryMinSearch ::
--   (Int -> Bool) ->
--   Int ->
--   Int ->
--   Maybe Int

           -- in length <$> bfs step 0 (== 70)
-- binarySearch ::
--   (Int -> Ordering) -> -- LT: Too small, GT: Too big
--   Int ->
--   Int ->
--   Maybe Int
-- binarySearch p = go
--   where
--     go !x !y
--       | x == y = if p x == EQ then Just x else Nothing
--       | otherwise = case p mid of
--           LT -> go mid y
--           EQ -> Just mid
--           GT -> go x mid
--       where
--         mid = ((y - x) `div` 2) + x

    }
