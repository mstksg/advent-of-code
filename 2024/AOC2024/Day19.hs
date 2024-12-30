-- |
-- Module      : AOC2024.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day19 (
  day19a,
  day19b,
)
where

import AOC.Common.Parser (pAlphaNumWord, parseMaybe')
import AOC.Solver (type (:~>) (..))
import Control.DeepSeq (NFData)
import Data.Finite (Finite)
import Data.Foldable (fold, toList)
import Data.Functor.Foldable hiding (fold)
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector.Sized as SV
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data NTrie n a = CT {ctHere :: Maybe a, ctThere :: SV.Vector n (NTrie n a)}
  deriving stock (Show, Functor, Traversable, Foldable, Generic)

deriving anyclass instance NFData a => NFData (NTrie n a)

makeBaseFunctor ''NTrie

fromMapCoalg ::
  forall n a.
  (KnownNat n, Semigroup a) =>
  Set [Finite n] ->
  Map [Finite n] a ->
  NTrieF n a (Map [Finite n] a)
fromMapCoalg mp0 = \ks ->
  let x = M.lookup [] ks
      reAdd = case x of
        Nothing -> id
        Just y -> SV.zipWith (M.unionWith (<>)) (M.fromSet (const y) <$> initialSplit)
   in CTF x $ reAdd (splitTrie ks)
  where
    initialSplit :: SV.Vector n (Set [Finite n])
    initialSplit = SV.generate \i ->
      S.fromDistinctAscList
        [ ks
        | k : ks <- toList mp0
        , k == i
        ]
    splitTrie :: Map [Finite n] a -> SV.Vector n (Map [Finite n] a)
    splitTrie mp = SV.generate \i ->
      M.fromDistinctAscList
        [ (ks, x)
        | (k : ks, x) <- M.toList mp
        , k == i
        ]

lookupAlg :: NTrieF n a ([Finite n] -> Maybe a) -> [Finite n] -> Maybe a
lookupAlg CTF{..} = \case
  [] -> ctHereF
  c : cs -> (ctThereF `SV.index` c) cs

buildable :: (KnownNat n, Semigroup a) => a -> Set [Finite n] -> [Finite n] -> Maybe a
buildable x0 mp0 = hylo lookupAlg (fromMapCoalg mp0) (M.fromSet (const x0) mp0)

day19 :: Semigroup w => w -> ([w] -> Int) -> ([String], [String]) :~> Int
day19 x agg =
  MkSol
    { sParse =
        parseMaybe' do
          ws <- pAlphaNumWord `P.sepBy` ","
          P.newline
          P.newline
          ls <- pAlphaNumWord `P.sepBy` P.newline
          pure (ws, ls)
    , sShow = show
    , sSolve = \(ws, ls) -> do
        ws' <- traverse toFinites ws
        ls' <- traverse toFinites ls
        pure $ agg $ mapMaybe (buildable x (S.fromList ws')) ls'
    }
  where
    toFinites :: String -> Maybe [Finite 5]
    toFinites = traverse $ flip M.lookup (M.fromList $ zip "wubrg" [0 ..])

day19a :: ([String], [String]) :~> Int
day19a = day19 () length

day19b :: ([String], [String]) :~> Int
day19b = day19 1 (getSum . fold)
