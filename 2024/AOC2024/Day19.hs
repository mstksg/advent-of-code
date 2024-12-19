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
import AOC.Solver (noFail, type (:~>) (..))
import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import Data.Functor.Foldable (Corecursive (ana), Recursive (cata))
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Semigroup (Sum (getSum))
import GHC.Generics (Generic)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data CharTrie a = CT {ctHere :: Maybe a, ctThere :: Map Char (CharTrie a)}
  deriving stock (Show, Functor, Traversable, Foldable, Generic)

deriving anyclass instance NFData a => NFData (CharTrie a)

makeBaseFunctor ''CharTrie

instance Semigroup a => Semigroup (CharTrie a) where
  CT h1 t1 <> CT h2 t2 = CT (h1 <> h2) (M.unionWith (<>) t1 t2)

instance Semigroup a => Monoid (CharTrie a) where
  mempty = CT Nothing M.empty

bindTrie :: Semigroup b => CharTrie a -> (a -> CharTrie b) -> CharTrie b
bindTrie t f = flip cata t \case
  CTF{..} -> case ctHereF of
    Nothing -> CT Nothing ctThereF
    Just x -> case f x of
      CT here' there' -> CT here' (M.unionWith (<>) ctThereF there')

singletonTrie :: String -> a -> CharTrie a
singletonTrie str x = flip ana str \case
  [] -> CTF (Just x) M.empty
  c : cs -> CTF Nothing (M.singleton c cs)

lookupTrie :: String -> CharTrie a -> Maybe a
lookupTrie str t = cata go t str
  where
    go CTF{..} = \case
      [] -> ctHereF
      c : cs -> ($ cs) =<< M.lookup c ctThereF

foreverTrie :: Semigroup w => [String] -> w -> CharTrie w
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
    , sSolve =
        noFail \(ws, ls) -> agg $ mapMaybe (`lookupTrie` foreverTrie ws x) ls
    }

day19a :: ([String], [String]) :~> Int
day19a = day19 () length

day19b :: ([String], [String]) :~> Int
day19b = day19 1 (getSum . fold)
