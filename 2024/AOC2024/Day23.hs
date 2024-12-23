{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day23 (
day23a,
day23b
)
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntMap.NonEmpty as IM
import qualified Data.Tree as Tree
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

day23a :: _ :~> _
day23a =
  MkSol
    { sParse = parseMaybe' $ sepByLines $ sequenceSepBy (V2 pAlphaNumWord  pAlphaNumWord) "-"
        -- noFail $
        --   lines
    , sShow = show
    , sSolve =
        noFail \xs ->
          let conns = M.unionsWith (<>)
                [ M.fromList [(a, S.singleton b), (b, S.singleton a)]
                  | V2 a b <- xs
                ]
              triples = flip M.mapWithKey conns \a bs ->
                              flip M.fromSet bs \b ->
                                S.filter ((a `S.member`) . (conns M.!)) $ conns M.! b
              tripleSet = S.fromList
                [ S.fromList [ a, b, c ]
                | (a, bs) <- M.toList triples
                , (b, cs) <- M.toList bs
                , c <- toList cs
                ]
           in countTrue (any ("t" `isPrefixOf`)) tripleSet
    }

-- data StringChain = 

day23b :: _ :~> _
day23b =
  MkSol
    { sParse = sParse day23a
    -- , sShow = unlines . map Tree.drawTree
    , sShow = intercalate "," . toList
    , sSolve =
        noFail \xs ->
          -- let gr :: G.Gr () ()
          --     gr = G.mkUGraph (foldMap (map encodeTwo . toList) xs) $
          --           [ (encodeTwo a, encodeTwo b)
          --             | V2 a b <- xs
          --           ]
          --     allNodes = foldMap (map encodeTwo . toList) xs
          --  in fmap decodeTwo <$> G.dff [encodeTwo "ka"] gr
          --  -- in map (map decodeTwo) $ G.scc gr
          let conns = M.unionsWith (<>)
                [ M.fromList [(a, S.singleton b), (b, S.singleton a)]
                  | V2 a b <- xs
                ]
              clubsTree = M.keys conns <&> \k ->
                  flip Tree.unfoldTree (k, S.empty) \(out, seen) ->
                      (out, [ (b, S.insert out seen)
                            | b <- toList $ conns M.! out
                            , all (< b) seen
                            , out < b
                            , all (b `S.member`) (map (conns M.!) $ toList seen)
                            ]
                      )
              clubs :: Set (Set String)
              clubs = fold $ fmap (go S.empty) clubsTree
                where
                  go :: Set String -> Tree.Tree String -> Set (Set String)
                  go seen (Tree.Node here there)
                    | null there = S.singleton (S.insert here seen)
                    | otherwise = fold $ go (S.insert here seen) <$> there
          in  toList $ maximumBy (comparing S.size) clubs
          -- in  clubsTree
    }
  where
    encodeTwo [a,b] = fromJust do 
      (_, a') <- charFinite a
      (_, b') <- charFinite b
      pure $ fromIntegral a' * 26 + fromIntegral b'
    decodeTwo ((`divMod` 26) -> (a,b)) = [review _CharFinite (False, fromIntegral a), review _CharFinite (False, fromIntegral b)]
-- charFinite :: Char -> Maybe (Bool, Finite 26)
