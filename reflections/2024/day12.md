First of all, let's assume we had a function that took a set and found all
contiguous regions of that set:

```haskell
contiguousRegions :: Set Point -> [Set Point]
```


Now we can take a `Map Point a` and then assume a map of a's to all of the
contiuous regions:

```haskell
regions :: Ord a => Map Point a -> Map a [Set Point]
regions mp =
  contiguousRegions
    <$> M.fromListWith (<>) [ (x, S.singleton p) | (p, x) <- M.toList mp ]
```

Now it helps to take a region and create four sets: the first, all of the
region's external neighbors to the north, the second, all of the region's
external enghbors to the west, then south, then east, etc.:

```haskell
neighborsByDir :: Set Point -> [Set Point]
neighborsByDir pts = neighborsAt <$> [V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0]
  where
    neighborsAt d = S.map (+ d) pts `S.difference` pts
```

Now part 1 basically is the size of all of those points, and part 2 is the
number of contiguous regions of those points:


```haskell
part1 :: Ord a => Map Point a -> Int
part1 = sum
    [ S.size region * S.size dirRegion
    | letterRegions <- regions mp
    , region <- letterRegions
    , dirRegion <- neighborsByDir region
    ]

part2 :: Ord a => Map Point a -> Int
part2 = sum
    [ S.size region * length (contiguousRegions dirRegion)
    | letterRegions <- regions mp
    , region <- letterRegions
    , dirRegion <- neighborsByDir region
    ]
```

Okay I'll admit that I had `contiguousRegions` saved from multiple years of
Advent of Code. The actual source isn't too pretty, but I'm including it here
for completion's sake. In my actual code I use set and non-empty set
instead of list and set.

```haskell
-- | Find contiguous regions by cardinal neighbors
contiguousRegions :: Set Point -> Set (NESet Point)
contiguousRegions = startNewPool S.empty
  where
    startNewPool seenPools remaining = case S.minView remaining of
      Nothing -> seenPools
      Just (x, xs) ->
        let (newPool, remaining') = fillUp (NES.singleton x) S.empty xs
         in startNewPool (S.insert newPool seenPools) remaining'
    fillUp boundary internal remaining = case NES.nonEmptySet newBoundary of
      Nothing -> (newInternal, remaining)
      Just nb -> fillUp nb (NES.toSet newInternal) newRemaining
      where
        edgeCandidates = foldMap' cardinalNeighbsSet boundary `S.difference` internal
        newBoundary = edgeCandidates `S.intersection` remaining
        newInternal = NES.withNonEmpty id NES.union internal boundary
        newRemaining = remaining `S.difference` edgeCandidates
```
