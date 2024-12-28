module AOC.Common.Search (
  aStar,
  bfs,
  bfsActions,
  binarySearch,
  exponentialSearch,
  binaryMinSearch,
  exponentialMinSearch,
  binaryFindMin,
  exponentialFindMin,
)
where

import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as Q
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (foldl')

data AStarState n p = AS
  { _asClosed :: !(Map n (Maybe n))
  -- ^ map of item to "parent"
  , _asOpen :: !(OrdPSQ n p (p, Maybe n))
  -- ^ map of item to "parent", and cost-so-far
  }

-- | A* Search
aStar ::
  forall n p.
  (Ord n, Ord p, Num p) =>
  -- | heuristic
  (n -> p) ->
  -- | neighborhood
  (n -> Map n p) ->
  -- | start
  n ->
  -- | target
  (n -> Bool) ->
  -- | the shortest path, if it exists, and its cost
  Maybe (p, [n])
aStar h ex x0 dest = second reconstruct <$> go (addBack x0 0 Nothing (AS M.empty Q.empty))
  where
    reconstruct :: (n, Map n (Maybe n)) -> [n]
    reconstruct (goal, mp) = reverse $ goreco goal
      where
        goreco n = n : maybe [] goreco (mp M.! n)
    go :: AStarState n p -> Maybe (p, (n, Map n (Maybe n)))
    go as0@AS{..} =
      Q.minView _asOpen >>= \(n, p, (g, up), queue') ->
        let closed' = M.insert n up _asClosed
         in if dest n
              then Just (p, (n, closed'))
              else
                go . M.foldlWithKey' (processNeighbor n g) (as0{_asOpen = queue', _asClosed = closed'}) $
                  ex n
    addBack :: n -> p -> Maybe n -> AStarState n p -> AStarState n p
    addBack x g up as0 = as0{_asOpen = insertIfBetter x (g + h x) (g, up) . _asOpen $ as0}
    processNeighbor :: n -> p -> AStarState n p -> n -> p -> AStarState n p
    processNeighbor curr currCost as0@AS{..} neighb moveCost
      --     | neighb `Q.member` _asOpen || neighb `M.member` _asClosed = as0
      | neighb `M.member` _asClosed = as0
      | otherwise = addBack neighb (currCost + moveCost) (Just curr) as0

insertIfBetter :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insertIfBetter k p x q = case Q.lookup k q of
  Nothing -> Q.insert k p x q
  Just (p', _)
    | p < p' -> Q.insert k p x q
    | otherwise -> q

data BFSState n = BS
  { _bsClosed :: !(Map n (Maybe n))
  -- ^ map of item to "parent"
  , _bsOpen :: !(Seq n)
  -- ^ queue
  }

-- | Breadth-first search, with loop detection
bfs ::
  forall n.
  Ord n =>
  -- | neighborhood
  (n -> Set n) ->
  -- | start
  n ->
  -- | target
  (n -> Bool) ->
  -- | the shortest path, if it exists
  Maybe [n]
bfs ex x0 dest = reconstruct <$> go (addBack x0 Nothing (BS M.empty Seq.empty))
  where
    reconstruct :: (n, Map n (Maybe n)) -> [n]
    reconstruct (goal, mp) = drop 1 . reverse $ goreco goal
      where
        goreco n = n : maybe [] goreco (mp M.! n)
    go :: BFSState n -> Maybe (n, Map n (Maybe n))
    go BS{..} = case _bsOpen of
      Empty -> Nothing
      n :<| ns
        | dest n -> Just (n, _bsClosed)
        | otherwise -> go . S.foldl' (processNeighbor n) (BS _bsClosed ns) $ ex n
    addBack :: n -> Maybe n -> BFSState n -> BFSState n
    addBack x up BS{..} =
      BS
        { _bsClosed = M.insert x up _bsClosed
        , _bsOpen = _bsOpen :|> x
        }
    processNeighbor :: n -> BFSState n -> n -> BFSState n
    processNeighbor curr bs0@BS{..} neighb
      | neighb `M.member` _bsClosed = bs0
      | otherwise = addBack neighb (Just curr) bs0

data BFSActionState a n = BAS
  { _basClosed :: !(Map n (Maybe (a, n)))
  -- ^ map of item to "parent"
  , _basOpen :: !(Seq n)
  -- ^ queue
  }

-- | Breadth-first search, with loop detection, that outputs actions
bfsActions ::
  forall a n.
  Ord n =>
  -- | neighborhood
  (n -> [(a, n)]) ->
  -- | start
  n ->
  -- | target
  (n -> Bool) ->
  -- | the shortest path, if it exists
  Maybe [a]
bfsActions ex x0 dest = reconstruct <$> go (addBack x0 Nothing (BAS M.empty Seq.empty))
  where
    reconstruct :: (n, Map n (Maybe (a, n))) -> [a]
    reconstruct (goal, mp) = reverse $ goreco goal
      where
        goreco n = case mp M.! n of
          Nothing -> []
          Just (act, n') -> act : goreco n'
    go :: BFSActionState a n -> Maybe (n, Map n (Maybe (a, n)))
    go BAS{..} = case _basOpen of
      Empty -> Nothing
      n :<| ns
        | dest n -> Just (n, _basClosed)
        | otherwise -> go . foldl' (processNeighbor n) (BAS _basClosed ns) $ ex n
    addBack :: n -> Maybe (a, n) -> BFSActionState a n -> BFSActionState a n
    addBack x up BAS{..} =
      BAS
        { _basClosed = M.insert x up _basClosed
        , _basOpen = _basOpen :|> x
        }
    processNeighbor :: n -> BFSActionState a n -> (a, n) -> BFSActionState a n
    processNeighbor curr bs0@BAS{..} (act,neighb)
      | neighb `M.member` _basClosed = bs0
      | otherwise = addBack neighb (Just (act, curr)) bs0

binarySearch ::
  (Int -> Ordering) -> -- LT: Too small, GT: Too big
  Int ->
  Int ->
  Maybe Int
binarySearch p = go
  where
    go !x !y
      | x == y = if p x == EQ then Just x else Nothing
      | otherwise = case p mid of
          LT -> go mid y
          EQ -> Just mid
          GT -> go x mid
      where
        mid = ((y - x) `div` 2) + x

exponentialSearch ::
  (Int -> Ordering) -> -- LT: Too small, GT: Too big
  Int ->
  Maybe Int
exponentialSearch p = go
  where
    go !x = case p x of
      LT -> go (x * 2)
      EQ -> Just x
      GT -> binarySearch p (x `div` 2) x

-- | Find the lowest value where the predicate is satisfied within the
-- given bounds.
binaryMinSearch ::
  (Int -> Bool) ->
  Int ->
  Int ->
  Maybe Int
binaryMinSearch p = go
  where
    go !x !y
      | x == mid || y == mid = Just (x + 1)
      | p mid = go x mid
      | otherwise = go mid y
      where
        mid = ((y - x) `div` 2) + x

-- | Find the lowest value where the predicate is satisfied above a given
-- bound.
exponentialMinSearch ::
  (Int -> Bool) ->
  Int ->
  Maybe Int
exponentialMinSearch p = go
  where
    go !x
      | p x = binaryMinSearch p (x `div` 2) x
      | otherwise = go (x * 2)

-- | Find the lowest value where the predicate is 'Just' within the
-- given bounds.
binaryFindMin ::
  (Int -> Maybe a) ->
  Int ->
  Int ->
  Maybe a
binaryFindMin p x0 y0 = binaryFindMin_ p (p y0) x0 y0

binaryFindMin_ ::
  (Int -> Maybe a) ->
  Maybe a ->
  Int ->
  Int ->
  Maybe a
binaryFindMin_ p = go
  where
    go found !x !y
      | x == mid || y == mid = found
      | otherwise = case p mid of
          Nothing -> go found mid y
          f@(Just _) -> go f x mid
      where
        mid = ((y - x) `div` 2) + x

-- | Find the lowest value where the predicate is 'Just' above a given
-- bound.
exponentialFindMin ::
  (Int -> Maybe a) ->
  Int ->
  Maybe a
exponentialFindMin p = go
  where
    go !x = case p x of
      Nothing -> go (x * 2)
      f@(Just _) -> binaryFindMin_ p f (x `div` 2) x
