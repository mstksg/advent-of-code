-- |
-- Module      : AOC2024.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day16 (
  day16a,
  day16b,
)
where

import AOC.Common.Point (Dir (..), Point, dirPoint, parseAsciiMap)
import AOC.Solver (noFail, type (:~>) (..))
import Data.Bifunctor (Bifunctor (second))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq
import Data.Set (Set)
import qualified Data.Set as S

solve :: Map Point (Maybe Bool) -> Maybe (Int, [Set Point])
solve mp = do
  (start, _) <- M.lookupMin $ M.filter (== Just False) mp
  (end, _) <- M.lookupMin $ M.filter (== Just True) mp
  let walls = M.keysSet $ M.filter (== Nothing) mp
      proj (p, d) = (p, d `elem` [North, South])
  second (map (S.map fst)) <$> allMinimalPaths proj (step walls) (start, East) ((== end) . fst)

day16a :: Map Point (Maybe Bool) :~> Int
day16a =
  MkSol
    { sParse =
        noFail $
          parseAsciiMap \case
            '#' -> Just Nothing
            'S' -> Just (Just False)
            'E' -> Just (Just True)
            _ -> Nothing
    , sShow = show
    , sSolve = fmap fst . solve
    }

day16b :: Map Point (Maybe Bool) :~> Int
day16b =
  MkSol
    { sParse = sParse day16a
    , sShow = show
    , sSolve = fmap (S.size . mconcat . snd) . solve
    }

step :: Set Point -> (Point, Dir) -> Map (Point, Dir) Int
step walls (p, d) =
  M.fromList
    [ ((p, d'), 1000)
    | d' <- [d <> West, d <> East]
    , (p + dirPoint d') `S.notMember` walls
    ]
    <> if p' `S.member` walls
      then mempty
      else M.singleton (p', d) 1
  where
    p' = p + dirPoint d

data Path n m p = Path {pCurr :: !n, pSeen :: !(Set m), pCost :: !p}
  deriving stock (Eq, Ord, Show)

allMinimalPaths ::
  forall n m p.
  (Ord n, Ord p, Num p, Ord m) =>
  -- | Duplicate projection
  (n -> m) ->
  -- | neighborhood
  (n -> Map n p) ->
  -- | start
  n ->
  -- | target
  (n -> Bool) ->
  -- | all paths with the shortest cost
  Maybe (p, [Set m])
allMinimalPaths proj expand start targ = go0 (M.singleton start path0) (M.singleton 0 (NESeq.singleton path0))
  where
    path0 = Path start S.empty 0
    go0 :: Map n (Path n m p) -> Map p (NESeq (Path n m p)) -> Maybe (p, [Set m])
    go0 bests queue = do
      ((p, Path{..} NESeq.:<|| xs), queue') <- M.minViewWithKey queue
      let queue'' = case NESeq.nonEmptySeq xs of
            Nothing -> queue'
            Just xs' -> M.insert p xs' queue'
      if targ pCurr
        then Just (p, pSeen : go1 p bests (M.takeWhileAntitone (<= p) queue''))
        else
          uncurry go0 . M.foldlWithKey' (processNeighbor pCost pSeen) (bests, queue'') $ expand pCurr
    go1 :: p -> Map n (Path n m p) -> Map p (NESeq (Path n m p)) -> [Set m]
    go1 minCost bests queue = case M.minViewWithKey queue of
      Nothing -> []
      Just ((p, Path{..} NESeq.:<|| xs), queue') ->
        let queue'' = case NESeq.nonEmptySeq xs of
              Nothing -> queue'
              Just xs' -> M.insert p xs' queue'
         in if targ pCurr
              then pSeen : go1 minCost bests queue''
              else
                uncurry (go1 minCost)
                  . second (M.takeWhileAntitone (<= minCost))
                  . M.foldlWithKey' (processNeighbor pCost pSeen) (bests, queue'')
                  $ expand pCurr
    processNeighbor ::
      p ->
      Set m ->
      (Map n (Path n m p), Map p (NESeq (Path n m p))) ->
      n ->
      p ->
      (Map n (Path n m p), Map p (NESeq (Path n m p)))
    processNeighbor cost seen (bests, queue) x newCost
      | proj x `S.member` seen = (bests, queue)
      | otherwise = case M.lookup x bests of
          Nothing -> (M.insert x newPath bests, newQueue)
          Just Path{..}
            | cost + newCost <= pCost -> (M.insert x newPath bests, newQueue)
            | otherwise -> (bests, queue)
      where
        newPath = Path x (S.insert (proj x) seen) (cost + newCost)
        newQueue =
          M.insertWith
            (flip (<>))
            (cost + newCost)
            (NESeq.singleton newPath)
            queue
