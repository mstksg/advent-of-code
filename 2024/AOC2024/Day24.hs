{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day24 (
  day24a,
  day24b,
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
import qualified Data.Tree as Tree
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

-- x00: 1
-- x01: 1
-- x02: 0
-- x03: 0
-- x04: 0
-- x05: 1
-- x06: 0
-- x07: 1
-- x08: 1
-- x09: 0

day24a :: _ :~> _
day24a =
  MkSol
    { sParse = parseMaybe' do
        cs <- P.many $ P.try $ do
          x <- pAlphaNumWord
          ":"
          P.space
          n <- pDecimal
          P.space
          pure (x, n)
        os <-
          P.many . P.try $
            (,)
              <$> (sequence (V3 pAlphaNumWord pAlphaNumWord pAlphaNumWord) <* "->")
              <*> (pAlphaNumWord <* P.space)
        pure (cs, os)
    , sShow = show
    , sSolve =
        noFail \(st, xs) ->
          let rules = M.fromList $ swap <$> xs
              starts = (== 1) <$> M.fromList st
              res = flip M.mapWithKey rules \x (V3 a b c) ->
                let y = M.findWithDefault (res M.! a) a starts
                    z = M.findWithDefault (res M.! c) c starts
                 in case b of
                      "AND" -> y && z
                      "OR" -> y || z
                      "XOR" -> y /= z
           in parseBinary . reverse . toList $ M.filterWithKey (\k _ -> "z" `isPrefixOf` k) res
    }

-- -- | each item paired with the list not including that item
-- select :: [a] -> [(a, [a])]
-- select = go []
--   where
--     go _ [] = []
--     go xs (y : ys) = (y, xs ++ ys) : go (y : xs) ys

day24b :: _ :~> _
day24b =
  MkSol
    { sParse = sParse day24a
    , sShow = ('\n' :)
    , -- , sShow = show
      sSolve =
        noFail \(st, xs) ->
          let rules = M.fromList $ swap <$> xs
              starts = flip Tree.Node [] . Right . (== 1) <$> M.fromList st
              -- pairs = flip evalStateT (M.keys starts) $
              --   replicateM 4 do
              --     x <- StateT select
              --     y <- StateT select
              --     pure (V2 x y)
              res =
                rules <&> \(V3 a b c) ->
                  -- let y = M.findWithDefault (res M.! a) a starts
                  --     z = M.findWithDefault (res M.! c) c starts
                  let y = maybe (Tree.Node (Right a) []) id $ M.lookup a res
                      -- M.findWithDefault (res M.! a) a starts
                      z = maybe (Tree.Node (Right c) []) id $ M.lookup c res
                   in -- z = M.findWithDefault (res M.! c) c starts
                      Tree.Node (Left b) [y, z]
           in -- case b of
              --         "AND" -> y && z
              --         "OR" -> y || z
              --         "XOR" -> y /= z
              -- res = rules <&> \(V3 a b c) ->
              --   let y = maybe (Left a) Right $ M.lookup a res
              --       z = maybe (Left c) Right $ M.lookup c res
              --   in  (b, y, z)
              -- case b of
              --         "AND" -> y && z
              --         "OR" -> y || z
              --         "XOR" -> y /= z
              Tree.drawForest $
                (\(k, v) -> Tree.Node k [either id id <$> v])
                  <$> M.toList (M.filterWithKey (\k _ -> "z" `isPrefixOf` k) res)
    }
