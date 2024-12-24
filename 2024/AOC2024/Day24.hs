{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE InstanceSigs #-}

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
module AOC2024.Day24 
  -- (
  -- day24a,
  -- day24b,
  -- adderTree,
  -- gateTreeTree,
-- )
where

import AOC.Prelude
import Control.Monad.Free
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
import GHC.Generics
import qualified Data.Tree as Tree
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Data.Functor.Classes
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

data Op = OAnd | OOr | OXor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data Gate a = Gate {gOp :: Op, gX :: a, gY :: a}
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

instance Show1 Gate where
  liftShowsPrec sp _ d (Gate o x y) = showsBinaryWith sp sp (show o) d x y

normalizeGate :: Ord a => Gate a -> Gate a
normalizeGate (Gate o x y)
  | x <= y = Gate o x y
  | otherwise = Gate o y x

instance Ord a => Eq (Gate a) where
  a == b = case (normalizeGate a, normalizeGate b) of
             (Gate o x y, Gate o' x' y') -> o == o' && x == x' && y == y'

instance Ord a => Ord (Gate a) where
  compare a b = case (normalizeGate a, normalizeGate b) of
    (Gate o x y, Gate o' x' y') -> mconcat [compare o o', compare x x', compare y y']


parseGate :: CharParser (Gate String)
parseGate = do
  gX <- pAlphaNumWord
  gOp <-
    P.choice
      [ OAnd <$ "AND"
      , OOr <$ "OR"
      , OXor <$ "XOR"
      ]
  gY <- pAlphaNumWord
  pure Gate{..}

parseInitial :: CharParser (String, Bool)
parseInitial = (,) <$> many P.alphaNumChar <* ": " <*> tokenAssoc [('0', False), ('1', True)]

applyOp :: Op -> Bool -> Bool -> Bool
applyOp = \case
  OAnd -> (&&)
  OOr -> (||)
  OXor -> (/=)

applyGate :: Gate Bool -> Bool
applyGate Gate{..} = applyOp gOp gX gY

day24a :: ([(String, Bool)], [(Gate String, String)]) :~> _
day24a =
  MkSol
    { sParse = parseMaybe' do
        cs <- P.many $ parseInitial <* P.newline
        P.newline
        os <- sepByLines $ (,) <$> parseGate <* "-> " <*> P.many P.alphaNumChar
        pure (cs, os)
    , sShow = show
    , sSolve =
        noFail \(st, xs) ->
          let rules = M.fromList $ swap <$> xs
              res = M.fromList st <> (applyGate . fmap (res M.!) <$> rules)
           in parseBinary . reverse . toList $ M.filterWithKey (\k _ -> "z" `isPrefixOf` k) res
    }

data Var = VX | VY | VZ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data VarBit = VB {vbVar :: Var, vbBit :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

showVarBit :: VarBit -> String
showVarBit VB{..} = printf (asString "%s%02d") vstr vbBit
  where
    vstr = asString case vbVar of
             VX -> "x"
             VY -> "y"
             VZ -> "z"

type GateTree = Free Gate

gateTreeTree :: GateTree a -> Tree.Tree (Either Op a)
gateTreeTree = iter (\Gate{..} -> Tree.Node (Left gOp) [gX, gY]) . fmap (flip Tree.Node [] . Right)

halfAdder :: GateTree a -> GateTree a -> (GateTree a, GateTree a)
halfAdder x y = (wrap $ Gate OAnd x y, wrap $ Gate OXor x y)

-- oh no we have some variation in ordering ....
fullAdder :: GateTree a -> GateTree a -> GateTree a -> (GateTree a, GateTree a)
fullAdder x y carry0 = (wrap $ Gate OOr carry1 carry2, o)
  where
    (carry1, z) = halfAdder x y
    (carry2, o) = halfAdder z carry0

adderTree :: Int -> (GateTree VarBit, NonEmpty (GateTree VarBit))
adderTree n
  | n == 0 = (:| []) `second` halfAdder (pure (VB VX 0)) (pure (VB VY 0))
  | otherwise =
      let (carryIn, rest) = adderTree (n - 1)
          (carryOut, new) = fullAdder (pure (VB VX n)) (pure (VB VY n)) carryIn
       in (carryOut, new `NE.cons` rest)

-- go maxBound
--   where

-- halfAdder :: Free Gate a
-- halfAdder =

-- strategy: traverse until you find a mistake
--
-- or maybe build bottom-up with the pieces available?
--
-- yeah build bottom-up sounds like a good idea. and then we can diff the two
-- trees via zip
day24b :: _ :~> _
day24b =
  MkSol
    { sParse = fmap snd . sParse day24a
    , sShow = ('\n' :)
      -- sShow = show
    , -- , sShow = show
      sSolve = noFail \ xs ->
            show $ M.fromList (first normalizeGate <$> xs)
        -- let rules = M.fromList $ swap <$> xs
        --     res :: Map String (GateTree String)
        --     res = wrap . fmap (\k -> M.findWithDefault (pure k) k res) <$> rules
        --     res' = M.filterWithKey (\k _ -> k == "z02") res
        -- in Tree.drawForest $ M.toList res' <&> \(k, v) -> Tree.Node k [either show id <$> gateTreeTree v]

            -- res =
            --   rules <&> \(V3 a b c) ->
            --     -- let y = M.findWithDefault (res M.! a) a starts
            --     --     z = M.findWithDefault (res M.! c) c starts
            --     let y = maybe (Tree.Node (Right a) []) id $ M.lookup a res
            --         -- M.findWithDefault (res M.! a) a starts
            --         z = maybe (Tree.Node (Right c) []) id $ M.lookup c res
            --      in -- z = M.findWithDefault (res M.! c) c starts
            --         Tree.Node (Left b) [y, z]
         -- in -- case b of
            -- --         "AND" -> y && z
            -- --         "OR" -> y || z
            -- --         "XOR" -> y /= z
            -- -- res = rules <&> \(V3 a b c) ->
            -- --   let y = maybe (Left a) Right $ M.lookup a res
            -- --       z = maybe (Left c) Right $ M.lookup c res
            -- --   in  (b, y, z)
            -- -- case b of
            -- --         "AND" -> y && z
            -- --         "OR" -> y || z
            -- --         "XOR" -> y /= z
            -- Tree.drawForest $
            --   (\(k, v) -> Tree.Node k [either id id <$> v])
            --     <$> M.toList (M.filterWithKey (\k _ -> "z" `isPrefixOf` k) res)
    }
