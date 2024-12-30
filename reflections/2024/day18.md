Honestly there really isn't much to this puzzle other than applying a basic BFS
to solve the maze. It isn't really even big enough that a-star would help.

If you parse the maze into an *fgl* graph, you can use something like `sp ::
Node -> Node -> gr a b -> Maybe Path` to get the shortest path. However,
because we're here anyway, I'm going to paste in my personal BFS code that I
use for these challenges that I wrote a while ago, where neighborhoods are
given by an `n -> Set n` function. It uses a `Seq` as its internal queue, which
is my favorite queue type in Haskell.

```haskell
data BFSState n = BS
  { _bsClosed :: !(Map n (Maybe n))
  -- ^ map of item to "parent"
  , _bsOpen :: !(Seq n)
  -- ^ queue
  }

bfs :: forall n. Ord n => (n -> Set n) -> n -> (n -> Bool) -> Maybe [n]
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

type Point = V2 Int

cardinalNeighbsSet :: Point -> Set Point
cardinalNeighbsSet p = S.fromDistinctAscList . map (p +) $
    [ V2 (-1) 0 , V2 0 (-1) , V2 0 1 , V2 1 0 ]

solveMaze :: Set Point -> Maybe Int
solveMaze walls = length <$> bfs step 0 (== 70)
  where
    step p = S.filter (all (inRange (0, 70))) $ cardinalNeighbsSet p `S.difference` walls
```

Now if you have a list of points `[Point]`, for part 1 you just solve the maze
after taking the first 1024 of them:

```haskell
part1 :: [Point] -> Maybe Int
part1 = solveMaze . S.fromList . take 1024
```

For part 2, you can search for the first success, or you can do a binary
search.

```haskell
-- | Find the lowest value where the predicate is satisfied within the
-- given bounds.
binaryMinSearch :: (Int -> Bool) -> Int -> Int -> Maybe Int
binaryMinSearch p = go
  where
    go !x !y
      | x == mid || y == mid = Just (x + 1)
      | p mid = go x mid
      | otherwise = go mid y
      where
        mid = ((y - x) `div` 2) + x
```

```haskell
part2 :: [Point] -> Maybe Int
part2 pts = do
    j <- binaryMinSearch (isNothing . solveMaze . (!! wallList)) 0 (length pts)
    pure $ pts !! (j - 1)
  where
    wallList = scanl (flip S.insert) S.empty pts
```

You should probably use a container type with better indexing than a list,
though.
