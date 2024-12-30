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
import Data.Char
import Data.Foldable (fold, toList)
import Data.Functor.Foldable hiding (fold)
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data CharTrie a = CT {ctHere :: Maybe a, ctThere :: IntMap (CharTrie a)}
  deriving stock (Show, Functor, Traversable, Foldable, Generic)

deriving anyclass instance NFData a => NFData (CharTrie a)

makeBaseFunctor ''CharTrie

fromMapCoalg ::
  forall a.
  (Semigroup a) =>
  Set [Int] ->
  Map [Int] a ->
  CharTrieF a (Map [Int] a)
fromMapCoalg mp0 = \ks ->
  let x = M.lookup [] ks
      reAdd = case x of
        Nothing -> id
        Just y -> IM.unionWith (M.unionWith (<>)) (M.fromSet (const y) <$> initialSplit)
   in CTF x $ reAdd (splitTrie ks)
  where
    initialSplit :: IntMap (Set [Int])
    initialSplit =
      S.fromDistinctDescList . ($ [])
        <$> IM.fromAscListWith
          (.)
          [ (k, (ks :))
          | k : ks <- toList mp0
          ]
    splitTrie :: Map [Int] a -> IntMap (Map [Int] a)
    splitTrie mp =
      M.fromDistinctDescList . ($ [])
        <$> IM.fromAscListWith
          (.)
          [ (k, ((ks, x) :))
          | (k : ks, x) <- M.toList mp
          ]

lookupAlg :: CharTrieF a ([Int] -> Maybe a) -> [Int] -> Maybe a
lookupAlg CTF{..} = \case
  [] -> ctHereF
  c : cs -> ($ cs) =<< IM.lookup c ctThereF

buildable :: (Semigroup a) => a -> Set [Int] -> [Int] -> Maybe a
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
    , sSolve = noFail \(ws, ls) ->
        agg $
          mapMaybe (buildable x (S.fromList (map ord <$> ws)) . map ord) ls
    }

day19a :: ([String], [String]) :~> Int
day19a = day19 () length

day19b :: ([String], [String]) :~> Int
day19b = day19 1 (getSum . fold)
