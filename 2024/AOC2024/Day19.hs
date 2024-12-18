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
import Control.Monad (guard)
import Data.Finite (Finite)
import Data.Foldable (fold)
import Data.Functor.Foldable (Corecursive (ana), Recursive (cata))
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Semigroup (Sum (getSum))
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

singletonTrie :: KnownNat n => [Finite n] -> a -> NTrie n a
singletonTrie str x = flip ana str \case
  [] -> CTF (Just x) (SV.replicate Nothing)
  c : cs -> CTF Nothing (SV.generate \i -> cs <$ guard (i == c))

lookupTrie :: [Finite n] -> NTrie n a -> Maybe a
lookupTrie str t = cata go t str
  where
    go CTF{..} = \case
      [] -> ctHereF
      c : cs -> ($ cs) =<< ctThereF `SV.index` c

foreverTrie :: (KnownNat n, Semigroup w) => [[Finite n]] -> w -> NTrie n w
foreverTrie strs x = infiniTrie
  where
    tr = foldMap (`singletonTrie` x) strs
    infiniTrie = singletonTrie [] x <> (tr `bindTrie` const infiniTrie)

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
        pure $ agg $ mapMaybe (`lookupTrie` (foreverTrie @5) ws' x) ls'
    }
  where
    toFinites = traverse $ flip M.lookup (M.fromList $ zip "wubrg" [0 ..])

day19a :: ([String], [String]) :~> Int
day19a = day19 () length

day19b :: ([String], [String]) :~> Int
day19b = day19 1 (getSum . fold)
