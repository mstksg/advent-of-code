-- |
-- Module      : AOC2024.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day24 (
  day24a,
  day24b,
)
where

import AOC.Common (asString, loopEither, parseBinary)
import AOC.Common.Parser (CharParser, pAlphaNumWord, parseMaybe', sepByLines, tokenAssoc)
import AOC.Solver (noFail, type (:~>) (..))
import Control.Applicative (Alternative (empty, many))
import Control.DeepSeq (NFData)
import Control.Lens ((%=))
import Control.Monad.Free (Free, MonadFree (wrap), iterA)
import Control.Monad.Logic (LogicT, MonadLogic (interleave), observeT)
import Control.Monad.State (MonadState (get, put), State, StateT, execState, execStateT)
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (Foldable (toList))
import Data.Generics.Labels ()
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (intercalate, isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple (swap)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Printf (printf)

data Op = OAnd | OOr | OXor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data Gate a = Gate {gOp :: Op, gX :: a, gY :: a}
  deriving stock (Show, Generic, Functor, Traversable, Foldable)
  deriving anyclass (NFData)

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

halfAdder :: GateTree a -> GateTree a -> (GateTree a, GateTree a)
halfAdder x y = (wrap $ Gate OAnd x y, wrap $ Gate OXor x y)

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

unrollGates ::
  forall a. Ord a => GateTree a -> State (Int, Map (Gate (Either Int a)) Int) (Either Int a)
unrollGates = iterA go . fmap Right
  where
    go g0 = do
      gate <- sequenceA g0
      (currIx, currMp) <- get
      case M.lookup gate currMp of
        Nothing -> do
          put (currIx + 1, M.insert gate currIx currMp)
          pure $ Left currIx
        Just i -> pure $ Left i

unrollAdderTree :: Int -> IntMap (Gate (Either Int VarBit))
unrollAdderTree n = IM.fromList $ swap <$> M.toList mp
  where
    (carry, adder) = adderTree n
    full = carry `NE.cons` adder
    (_, mp) = execState (traverse unrollGates full) (0, M.empty)

data NameState = NS
  { nsRenames :: Map String String
  , nsNames :: IntMap String
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData)

nameGate ::
  Map (Gate String) String ->
  Int ->
  Int ->
  Gate (Either Int VarBit) ->
  LogicT (StateT NameState Maybe) String
nameGate avail renameLimit ng Gate{..} = case (gX, gY) of
  (Left i, Left j) -> do
    NS{..} <- get
    let nX = nsNames IM.! i
        nY = nsNames IM.! j
        gate = Gate gOp nX nY
    Just here <- pure $ applySwaps nsRenames <$> M.lookup gate avail
    (here <$ (#nsNames %= IM.insert ng here))
      `interleave` foldr
        interleave
        empty
        [ there <$ put (NS renames (IM.insert ng there nsNames))
        | here `M.notMember` nsRenames
        , here `notElem` nsNames
        , M.size nsRenames < renameLimit
        , there <- toList avail
        , here /= there
        , there `M.notMember` nsRenames
        , there `notElem` nsNames
        , let renames = M.fromList [(here, there), (there, here)] <> nsRenames
        ]
  (Right x, Right y) -> do
    NS{..} <- get
    let gate = showVarBit <$> Gate gOp x y
    Just here <- pure $ applySwaps nsRenames <$> M.lookup gate avail
    (here <$ (#nsNames %= IM.insert ng here))
      `interleave` foldr
        interleave
        empty
        [ there <$ put (NS renames (IM.insert ng there nsNames))
        | here `M.notMember` nsRenames
        , here `notElem` nsNames
        , M.size nsRenames < renameLimit
        , there <- toList avail
        , here /= there
        , there `M.notMember` nsRenames
        , there `notElem` nsNames
        , let renames = M.fromList [(here, there), (there, here)] <> nsRenames
        ]
  _ -> empty
  where
    applySwaps :: Map String String -> String -> String
    applySwaps mp x = M.findWithDefault x x mp

nameTree ::
  Map (Gate String) String ->
  Map String String ->
  IntMap (Gate (Either Int VarBit)) ->
  Maybe (Map String String)
nameTree avail renames0 =
  fmap nsRenames
    . flip execStateT s0
    . observeT
    . IM.traverseWithKey (nameGate avail (min 8 $ M.size renames0 + 2))
  where
    s0 = NS renames0 IM.empty

day24b :: [(Gate String, String)] :~> [String]
day24b =
  MkSol
    { sParse = fmap snd . sParse day24a
    , sShow = intercalate ","
    , sSolve = noFail \xs ->
        flip loopEither (0, M.empty) \(i, subs) ->
          case nameTree (M.fromList xs) subs (unrollAdderTree i) of
            Nothing -> Left $ M.keys subs
            Just subs' -> Right (i + 1, subs')
    }
