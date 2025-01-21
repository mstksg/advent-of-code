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

import AOC.Common.Parser (CharParser, pAlphaNumWord, parseMaybe', sepByLines, tokenAssoc)
import AOC.Solver (noFail, type (:~>) (..))
import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Control.Monad.Free (Free, MonadFree (wrap), iterA)
import Control.Monad.State (MonadState (get, put), State, runState)
import Data.Bifunctor (Bifunctor (second))
import Data.Either (lefts)
import Data.Foldable (Foldable (toList))
import Data.Generics.Labels ()
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
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
parseInitial = (,) <$> P.many P.alphaNumChar <* ": " <*> tokenAssoc [('0', False), ('1', True)]

applyOp :: Op -> Bool -> Bool -> Bool
applyOp = \case
  OAnd -> (&&)
  OOr -> (||)
  OXor -> (/=)

applyGate :: Gate Bool -> Bool
applyGate Gate{..} = applyOp gOp gX gY

day24a :: ([(String, Bool)], [(Gate String, String)]) :~> Int
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
           in sum [2 ^ read @Int n | ('z' : n, True) <- M.toList res]
    }

type GateTree = Free Gate

halfAdder :: GateTree a -> GateTree a -> (GateTree a, GateTree a)
halfAdder x y = (wrap $ Gate OAnd x y, wrap $ Gate OXor x y)

fullAdder :: GateTree a -> GateTree a -> GateTree a -> (GateTree a, GateTree a)
fullAdder x y carry0 = (wrap $ Gate OOr carry1 carry2, o)
  where
    (carry1, z) = halfAdder x y
    (carry2, o) = halfAdder z carry0

adderTree :: Int -> (GateTree String, NonEmpty (GateTree String))
adderTree n
  | n == 0 = (:| []) `second` halfAdder (pure "x00") (pure "y00")
  | otherwise =
      let (carryIn, rest) = adderTree (n - 1)
          (carryOut, new) = fullAdder (pure (printf "x%02d" n)) (pure (printf "y%02d" n)) carryIn
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

unrollAdderTree :: Int -> ([Int], IntMap (Gate (Either Int String)))
unrollAdderTree n = (lefts $ toList outs, IM.fromList $ swap <$> M.toList mp)
  where
    (carry, adder) = adderTree n
    full = NE.reverse $ carry `NE.cons` adder
    (outs, (_, mp)) = runState (traverse unrollGates full) (0, M.empty)

data NameState = NS
  { nsRenames :: Map String String
  , nsNames :: IntMap String
  , nsFound :: Bool
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData)

nameGate :: Map (Gate String) String -> Int -> Gate (Either Int String) -> NameState -> [NameState]
nameGate avail ng g0 NS{..} =
  case applySwaps nsRenames <$> M.lookup gate avail of
    Nothing -> []
    Just here ->
      NS{nsNames = IM.insert ng here nsNames, ..}
        : [ NS renames (IM.insert ng there nsNames) True
          | not nsFound
          , there <- toList avail
          , here /= there
          , let renames = M.fromList [(here, there), (there, here)] <> nsRenames
          ]
  where
    gate = either (nsNames IM.!) id <$> g0
    applySwaps mp x = M.findWithDefault x x mp

nameTree :: Map (Gate String) String -> [Map String String]
nameTree avail = nsRenames <$> foldr (\o -> (go o >=>)) pure outGates s0
  where
    s0 = NS M.empty IM.empty False
    (outGates, gates) = unrollAdderTree 44
    go outGate ns0
      | M.size (nsRenames ns0) == 8 = [ns0]
      | otherwise =
          IM.foldrWithKey
            (\k g -> (nameGate avail k g >=>))
            pure
            (IM.takeWhileAntitone (<= outGate) gates)
            (ns0{nsFound = False})

day24b :: [(Gate String, String)] :~> [String]
day24b =
  MkSol
    { sParse = fmap snd . sParse day24a
    , sShow = intercalate ","
    , sSolve = fmap M.keys . listToMaybe . nameTree . M.fromList
    }
