Nothing too deep I could think of for this one other than a specialized
dijkstra BFS, that initially acts like normal dijkstra until the first
successful path is found: after that, it treats that as the best cost, and it
only re-adds points back to the queue if the cost is less than the known best
cost.

```haskell
data Path n p = Path {pCurr :: n, pSeen :: Set n, pCost :: p}
  deriving stock (Eq, Ord, Show)

allMinimalPaths ::
  forall n p.
  (Ord n, Ord p, Num p) =>
  -- | neighborhood
  (n -> Map n p) ->
  -- | start
  n ->
  -- | target
  (n -> Bool) ->
  -- | all paths with the shortest cost
  Maybe (p, [Set n])
allMinimalPaths expand start targ = go0 (M.singleton start path0) (M.singleton 0 (NESeq.singleton path0))
  where
    path0 = Path start S.empty 0
    go0 :: Map n (Path n p) -> Map p (NESeq (Path n p)) -> Maybe (p, [Set n])
    go0 bests queue = do
      ((p, Path{..} NESeq.:<|| xs), queue') <- M.minViewWithKey queue
      let queue'' = case NESeq.nonEmptySeq xs of
            Nothing -> queue'
            Just xs' -> M.insert p xs' queue'
      if targ pCurr
        then Just (p, pSeen : go1 p bests (M.takeWhileAntitone (<= p) queue''))
        else
          uncurry go0 . M.foldlWithKey' (processNeighbor pCost pSeen) (bests, queue'') $ expand pCurr
    go1 :: p -> Map n (Path n p) -> Map p (NESeq (Path n m p)) -> [Set n]
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
      Set n ->
      (Map n (Path n p), Map p (NESeq (Path n p))) ->
      n ->
      p ->
      (Map n (Path n p), Map p (NESeq (Path n p)))
    processNeighbor cost seen (bests, queue) x newCost
      | x `S.member` seen = (bests, queue)
      | otherwise = case M.lookup x bests of
          Nothing -> (M.insert x newPath bests, newQueue)
          Just Path{..}
            | cost + newCost <= pCost -> (M.insert x newPath bests, newQueue)
            | otherwise -> (bests, queue)
      where
        newPath = Path x (S.insert x seen) (cost + newCost)
        newQueue =
          M.insertWith
            (flip (<>))
            (cost + newCost)
            (NESeq.singleton newPath)
            queue
```

Then we can solve part 1 and part 2 with the same search:

```haskell
type Point = V2 Int

type Dir = Finite 4

dirPoint :: Dir -> Point
dirPoint = SV.index $ SV.fromTuple (V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0)

step :: Set Point -> (Point, Dir) -> Map (Point, Dir) Int
step walls (p, d) =
  M.fromList
    [ ((p, d'), 1000)
    | d' <- [d + 1, d - 1]
    , (p + dirPoint d') `S.notMember` walls
    ]
    <> if p' `S.member` walls
      then mempty
      else M.singleton (p', d) 1
  where
    p' = p + dirPoint d

solve :: Set Point -> Point -> Point -> Maybe (Int, [Set Point])
solve walls start end = 
  second (map (S.map fst)) <$> allMinimalPaths proj (step walls) (start, East) ((== end) . fst)

part1 :: Set Point -> Point -> Point -> Maybe Int
part1 walls start end = fst <$> solve walls start end

part2 :: Set Point -> Point -> Point -> Maybe Int
part2 walls start end = S.size mconcat . snd <$> solve walls start end
```

Right now we consider two nodes to be the same if they have the same position
and the same direction, but there's a slight optimization we can do if we
consider them to be the same if they are in the same position and on the same
axis (going north/south vs going east/west) since it closes off paths that
backtrack.  However in practice this isn't really a big savings (5% for me).
