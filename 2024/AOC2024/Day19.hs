-- |
-- Module      : AOC2024.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day19
where

-- (
-- day19a,
-- day19b,
-- )

import AOC.Common.Parser (pAlphaNumWord, parseMaybe')
import AOC.Solver (type (:~>) (..))
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad (guard)
import Control.Monad.Free
import Data.Bifunctor
import Data.Finite (Finite)
import Data.Foldable
import Data.Foldable (fold)
import Data.Functor.Foldable hiding (fold)
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEM
import Data.Maybe (mapMaybe)
import Data.Semigroup
import Data.These
import qualified Data.Vector.Sized as SV
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data NTrie n a = CT {ctHere :: Maybe a, ctThere :: SV.Vector n (Maybe (NTrie n a))}
  deriving stock (Show, Functor, Traversable, Foldable, Generic)

deriving anyclass instance NFData a => NFData (NTrie n a)

makeBaseFunctor ''NTrie

instance Semigroup a => Semigroup (NTrie n a) where
  CT h1 t1 <> CT h2 t2 = CT (h1 <> h2) (SV.zipWith (<>) t1 t2)

instance (KnownNat n, Semigroup a) => Monoid (NTrie n a) where
  mempty = CT Nothing (SV.replicate Nothing)

bindTrie :: Semigroup b => NTrie n a -> (a -> NTrie n b) -> NTrie n b
bindTrie t f = flip cata t \case
  CTF{..} -> case ctHereF of
    Nothing -> CT Nothing ctThereF
    Just x -> case f x of
      CT here' there' -> CT here' (SV.zipWith (<>) ctThereF there')

fromMap :: KnownNat n => Map [Finite n] a -> NTrie n a
fromMap = ana \mp ->
  let (here, there) = flip M.foldMapWithKey mp \case
        [] -> \x -> (Just (First x), mempty)
        k : ks -> \x -> (mempty, SV.generate \i -> [(ks, x)] <$ guard (i == k))
   in CTF (getFirst <$> here) (fmap M.fromList <$> there)

fromMapForever ::
  forall n a. (KnownNat n, Semigroup a) => NEMap (NonEmpty (Finite n)) a -> NTrie n a
fromMapForever mp0 = ana (fromMapCoalg mp0) (That mp0)

fromMapCoalg ::
  forall n a.
  (KnownNat n, Semigroup a) =>
  NEMap (NonEmpty (Finite n)) a ->
  These a (NEMap (NonEmpty (Finite n)) a) ->
  NTrieF n a (These a (NEMap (NonEmpty (Finite n)) a))
fromMapCoalg mp0 = \case
  This x -> CTF (Just x) $ fmap separateMap <$> initialSplit x
  That ks -> CTF Nothing $ fmap separateMap <$> splitTrie ks
  These x ks ->
    CTF (Just x) $
      fmap separateMap <$> SV.zipWith combineMaybe (initialSplit x) (splitTrie ks)
  where
    initialSplit :: a -> SV.Vector n (Maybe (NEMap [Finite n] a))
    initialSplit x = fmap (x <$) <$> splitTrie mp0
    splitTrie :: NEMap (NonEmpty (Finite n)) a -> SV.Vector n (Maybe (NEMap [Finite n] a))
    splitTrie mp = SV.generate \i ->
      NEM.nonEmptyMap $
        M.fromListWith
          (<>)
          [ (ks, x)
          | (k :| ks, x) <- toList $ NEM.toList mp
          , k == i
          ]
    combineMaybe Nothing Nothing = Nothing
    combineMaybe (Just x) Nothing = Just x
    combineMaybe Nothing (Just y) = Just y
    combineMaybe (Just x) (Just y) = Just $ NEM.unionWith (<>) x y

separateMap :: NEMap [Finite n] a -> These a (NEMap (NonEmpty (Finite n)) a)
separateMap = first getFirst . NEM.foldMapWithKey go
  where
    go = \case
      [] -> This . First
      k : ks -> That . NEM.singleton (k :| ks)

trieFromList :: KnownNat n => [([Finite n], a)] -> NTrie n a
trieFromList = ana \mp ->
  let (here, there) = flip foldMap mp \case
        ([], x) -> (Just (First x), mempty)
        (k : ks, x) -> (mempty, SV.generate \i -> [(ks, x)] <$ guard (i == k))
   in CTF (getFirst <$> here) there

-- trieFromListForever :: KnownNat n => [([Finite n], a)] -> NTrie n a
-- trieFromListForever mp0 = flip ana mp0 \mp ->
--   let (here, there) = flip foldMap mp \case
--         ([], x) -> (Just (First x), SV.replicate (Just (first _ <$> mp0)))
--         (k : ks, x) -> (mempty, SV.generate \i -> [(ks, x)] <$ guard (i == k))
--    in CTF (getFirst <$> here) there

singletonTrie :: KnownNat n => [Finite n] -> a -> NTrie n a
singletonTrie str x = flip ana str \case
  [] -> CTF (Just x) (SV.replicate Nothing)
  c : cs -> CTF Nothing (SV.generate \i -> cs <$ guard (i == c))

lookupTrie :: [Finite n] -> NTrie n a -> Maybe a
lookupTrie str t = cata lookupAlg t str

lookupAlg :: NTrieF n a ([Finite n] -> Maybe a) -> [Finite n] -> Maybe a
lookupAlg CTF{..} = \case
  [] -> ctHereF
  c : cs -> ($ cs) =<< ctThereF `SV.index` c

foreverTrie' :: (KnownNat n, Semigroup w) => NonEmpty (NonEmpty (Finite n)) -> w -> NTrie n w
foreverTrie' strs x = fromMapForever $ NEM.fromList ((,x) <$> strs)

foreverTrie :: (KnownNat n, Semigroup w) => [[Finite n]] -> w -> NTrie n w
foreverTrie strs x = infiniTrie
  where
    tr = trieFromList $ (,x) <$> strs
    infiniTrie = singletonTrie [] x <> (tr `bindTrie` const infiniTrie)

buildable :: (KnownNat n, Semigroup a) => NEMap (NonEmpty (Finite n)) a -> [Finite n] -> Maybe a
buildable mp0 = hylo lookupAlg (fromMapCoalg mp0) (That mp0)

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
        ws' <- NE.nonEmpty =<< traverse (NE.nonEmpty <=< toFinites) ws
        ls' <- traverse toFinites ls
        pure $ agg $ mapMaybe (`lookupTrie` (foreverTrie' @5) ws' x) ls'
    }
  where
    toFinites = traverse $ flip M.lookup (M.fromList $ zip "wubrg" [0 ..])

day19a :: ([String], [String]) :~> Int
day19a = day19 () length

day19b :: ([String], [String]) :~> Int
day19b = day19 1 (getSum . fold)
