-- |
-- Module      : AOC2018.Day07
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
module AOC2018.Day07 (
  day07a,
  day07b,
) where

import AOC.Solver (dyno_, (:~>) (..))
import Control.Lens
import Control.Monad.RWS (MonadReader (..), MonadState (..), MonadWriter (..), runRWS)
import Data.Bifunctor (second)
import Data.Char (isUpper, ord)
import Data.Foldable (find, fold, forM_, toList)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup (Sum (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import Data.Tuple (swap)
import Numeric.Natural (Natural)
import Witherable (mapMaybe)

parseAll :: String -> Maybe (Map Char (Set Char))
parseAll =
  fmap (M.fromListWith (<>) . (map . second) S.singleton)
    . traverse parseLine
    . lines

parseLine :: String -> Maybe (Char, Char)
parseLine = pack . filter isUpper
  where
    pack [_, a, b] = Just (a, b)
    pack _ = Nothing

makeDeps :: (Ord a, Ord b) => Map a (Set b) -> Map b (NESet a)
makeDeps =
  M.fromListWith (<>)
    . map (second NES.singleton . swap)
    . concatMap (traverse toList)
    . M.toList

findRoots :: Ord a => Map a (Set a) -> Set a
findRoots mp = cs `S.difference` targs
  where
    cs = M.keysSet mp
    targs = S.unions $ toList mp

lexicoTopo :: Ord a => Map a (Set a) -> [a]
lexicoTopo childs = go (makeDeps childs) (findRoots childs)
  where
    go deps active = flip foldMap (find (`M.notMember` deps) active) $ \c ->
      let newDeps = mapMaybe (NES.nonEmptySet . NES.delete c) deps
          newActive =
            maybe id (<>) (M.lookup c childs)
              . S.delete c
              $ active
       in c : go newDeps newActive

day07a :: Map Char (Set Char) :~> String
day07a =
  MkSol
    { sParse = parseAll
    , sShow = id
    , sSolve = Just . lexicoTopo
    }

data Env a = Env
  { _envCap :: Int
  , _envWaiter :: a -> Natural
  }

makeLenses ''Env

data Scheduler a = MkSched
  { _schedQueue :: !(Set a)
  , _schedActive :: !(Map a Natural)
  }

makeClassy ''Scheduler

buildSleigh ::
  forall a m.
  ( Ord a
  , MonadState (Scheduler a) m
  , MonadReader (Env a) m
  , MonadWriter (Sum Natural) m
  ) =>
  Map a (Set a) ->
  m ()
buildSleigh childs = go (findRoots childs) (makeDeps childs)
  where
    go :: Set a -> Map a (NESet a) -> m ()
    go toAdd deps = do
      popped <- stepScheduler toAdd
      forM_ (NES.nonEmptySet popped) $ \popped' ->
        let newDeps = mapMaybe (NES.nonEmptySet . (`NES.difference` popped')) deps
            newToAdd =
              S.filter (`M.notMember` newDeps)
                . foldMap (fold . (`M.lookup` childs))
                $ popped'
         in go newToAdd newDeps

day07b :: Map Char (Set Char) :~> Natural
day07b =
  MkSol
    { sParse = parseAll
    , sShow = show
    , sSolve = \mp ->
        Just $
          let env =
                Env
                  { _envCap = dyno_ "cap" 5
                  , _envWaiter =
                      fromIntegral
                        . (+ 1)
                        . (+ dyno_ "wait" 60)
                        . subtract (ord 'A')
                        . ord
                  }
           in getSum . view _3 . runRWS (buildSleigh mp) env $ emptyScheduler
    }

-- | Scheduler Implementation
emptyScheduler :: Scheduler a
emptyScheduler = MkSched S.empty M.empty

stepScheduler ::
  ( Ord a
  , HasScheduler s a
  , MonadState s m
  , MonadReader (Env a) m
  , MonadWriter (Sum Natural) m
  ) =>
  Set a ->
  m (Set a) -- if empty, it means scheduler is exhausted
stepScheduler new = do
  cap <- view envCap
  waiter <- view envWaiter
  schedQueue <>= new
  numToAdd <- uses schedActive $ (cap -) . M.size
  toAdd <- schedQueue %%= S.splitAt numToAdd
  active <- schedActive <<>= M.fromSet waiter toAdd
  case NE.groupWith snd . sortOn snd $ M.toList active of
    [] -> pure S.empty
    toPop@((_, popTime) :| _) : stillActive -> do
      schedActive
        .= ( M.map (subtract popTime)
              . M.fromDistinctAscList
              . concatMap toList
              $ stillActive
           )
      tell $ Sum popTime
      pure $ S.fromDistinctAscList . map fst . toList $ toPop
