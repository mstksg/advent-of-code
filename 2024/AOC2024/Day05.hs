{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day05
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
module AOC2024.Day05 (
  day05a,
  day05b,
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

middleVal :: Seq a -> a
middleVal xs = Seq.index xs (Seq.length xs `div` 2)

day05a :: ([V2 Int], [Seq Int]) :~> _
day05a =
  MkSol
    { sParse = parseMaybe' do
        rules <- many $ P.try . fullLine . P.try $ sequenceSepBy (V2 pDecimal pDecimal) "|"
        P.newline
        pages <- map Seq.fromList <$> ((pDecimal `sepBy'` ",") `sepBy'` P.newline)
        pure (rules, pages)
    , sShow = show
    , sSolve =
        noFail \(rules, pages) ->
          let good xs = flip all rules \(V2 a b) -> fromMaybe True do
                  i <- M.lookup a ixMap
                  j <- M.lookup b ixMap
                  pure $ i < j
                where
                  ixMap = M.fromList $ zip (toList xs) [0..]
          in  sum . map middleVal . filter good $ pages
    }

day05b :: _ :~> _
day05b =
  MkSol
    { sParse = sParse day05a
    , sShow = show
    , sSolve =
        noFail \(rules, pages) ->
          let good xs = flip all rules \(V2 a b) -> fromMaybe True do
                  i <- M.lookup a ixMap
                  j <- M.lookup b ixMap
                  pure $ i < j
                where
                  ixMap = M.fromList $ zip (toList xs) [0..]
              rulesGraph = G.mkGraph @G.Gr
                  (nubOrd [ (x, ()) | x <- foldMap toList rules ])
                  [ (x, y, ()) | V2 x y <- rules ]
              buildUp xs = Seq.fromList . G.topsort . G.nfilter (`S.member` xs) $ rulesGraph
          -- let mkGood xs = xs' <$ guard (xs /= xs')
          --       -- flip all rules \(V2 a b) -> fromMaybe True do
          --       --   i <- M.lookup a ixMap
          --       --   j <- M.lookup b ixMap
          --       --   pure $ i < j
          --       where
          --         ixMap = M.fromList $ zip (toList xs) [0..]
          --         ixMap' = flip execState ixMap $ for (rules) \(V2 a b) -> modify \s ->
          --           fromMaybe s do
          --             i <- M.lookup a s
          --             j <- M.lookup b s
          --             guard (j > i)
          --             pure $ M.fromList [(b,i),(a,j)] <> s
          --         xs' = Seq.fromList . map fst . sortBy (comparing snd) $ M.toList ixMap'
          in  sum . map (middleVal . buildUp . S.fromList . toList) . filter (not . good) $ pages
    }
