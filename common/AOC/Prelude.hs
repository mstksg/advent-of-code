-- |
-- Module      : AOC.Prelude
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Custom Prelude while developing challenges.  Ideally, once challenges
-- are completed, an import to this module would be replaced with explicit
-- ones for future readers.
module AOC.Prelude (
  module P,
)
where

import AOC.Common as P
import AOC.Common.Parser as P
import AOC.Common.Point as P
import AOC.Common.Search as P
import AOC.Solver as P
import AOC.Util as P
import Control.Applicative as P
import Control.DeepSeq as P
import Control.Lens as P hiding (uncons)
import Control.Monad as P
import Control.Monad.Except as P
import Control.Monad.State as P
import Data.Bifunctor as P
import Data.Char as P
import Data.Containers.ListUtils as P
import Data.Either as P
import Data.Finite as P (Finite, finites, getFinite, modulo, packFinite)
import Data.Foldable as P
import Data.Function as P
import Data.Functor as P hiding (unzip)
import Data.Functor.Foldable as P (ana, cata, hylo)
import Data.Functor.Foldable.TH as P (makeBaseFunctor)
import Data.Functor.Rep as P (Representable(tabulate))
import Data.Generics.Labels ()
import Data.IntMap as P (IntMap)
import Data.IntMap.NonEmpty as P (NEIntMap)
import Data.IntSet as P (IntSet)
import Data.IntSet.NonEmpty as P (NEIntSet)
import Data.Ix as P hiding (index)
import Data.Kind as P
import Data.List as P (
  group,
  groupBy,
  inits,
  intercalate,
  intersperse,
  isInfixOf,
  isPrefixOf,
  isSubsequenceOf,
  isSuffixOf,
  permutations,
  scanl',
  sort,
  sortBy,
  sortOn,
  stripPrefix,
  subsequences,
  tails,
  transpose,
  uncons,
  unfoldr,
  unzip4,
  unzip5,
  unzip6,
  unzip7,
  zip4,
  zip5,
  zip6,
  zip7,
  zipWith4,
  zipWith5,
  zipWith6,
  zipWith7,
 )
import Data.List.NonEmpty as P (NonEmpty (..), nonEmpty)
import Data.List.Split as P
import Data.Map as P (Map)
import Data.Map.NonEmpty as P (NEMap)
import Data.Maybe as P
import Data.Ord as P
import Data.Semigroup as P
import Data.Sequence as P (Seq ((:<|), (:|>)))
import Data.Set as P (Set)
import Data.Set.NonEmpty as P (NESet)
import Data.Text as P (Text)
import Data.Text.Encoding as P (decodeUtf8, encodeUtf8)
import Data.Time as P hiding (Day)
import Data.Traversable as P
import Data.Tuple as P
import Data.Void as P
import Debug.Trace as P
import GHC.Generics as P (Generic)
import Numeric.Natural as P
import Safe as P (
  atMay,
  foldl1May',
  headMay,
  initMay,
  lastMay,
  predMay,
  scanl1May,
  succMay,
  tailMay,
 )
import Safe.Foldable as P (foldl1May, foldr1May, maximumByMay, maximumMay, minimumByMay, minimumMay)
import Text.Printf as P
import Text.Read as P (readMaybe)
