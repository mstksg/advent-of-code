Because this is a "race track" with no branching, finding the path to the end
can be a straightforward DFS with no-takebacksies:

```haskell
cardinalNeighbsSet :: Point -> Set Point
cardinalNeighbsSet p = S.fromDistinctAscList . map (p +) $
    [ V2 (-1) 0 , V2 0 (-1) , V2 0 1 , V2 1 0 ]

racePath :: Set Point -> Point -> Point -> Maybe [Point]
racePath walls start end = go Nothing start
  where
    go :: Maybe Point -> Point -> Maybe [Point]
    go prev here = do
      next <- S.lookupMin candidates
      (here :)
        <$> if next == end
          then pure [end]
          else go (Just here) next
      where
        candidates = maybe id S.delete prev $ cardinalNeighbsSet here `S.difference` walls
```

Since our racepath is one continuous line, a cheat therefore involves
"pinching" the line so that you skip straight over one segment of the line. So,
we can basically iterate over each point in the line and imagine if we jumped
ahead N spaces. If the time saved by jumping N spaces minus the real-world
distance is greater than the threshold, it's a legitimate cheat.

```haskell
mannDist :: Point -> Point
mannDist x y = sum (abs (x - y))

mannNorm :: Point -> Int
mannNorm = mannDist 0

findCheats :: Set Point -> Point -> Point -> Int -> Int -> Maybe Int
findCheats walls start end len thresh = do
  path <- racePath walls start end
  pure . sum . snd $ mapAccumR go (0, M.empty) path
  where
    go :: (Int, Map Point Int) -> Point -> ((Int, Map Point Int), Int)
    go (i, xs) x =
      ( (i + 1, M.insert x i xs)
      , M.size $
          M.filterWithKey (\y j -> i - j - mannDist x y >= thresh) $
            xs `M.restrictKeys` S.mapMonotonic (+ x) diamond
      )
    diamond = floodFill (S.filter ((<= len) . mannNorm) . cardinalNeighbsSet) (S.singleton 0)
```

Our `mapAccumR` here iterates from the end of the list with the index (`i`) and
a map `xs` of points to the index where that point is on the racetrack. At each
point, we output the number of cheats: it's the `xs` filtered by points legally
jumpable within a given distance, and then further filtered where the jump in
index `i - j` minus the time to travel `mannDist x y` is greater than the
threshold for counting the cheat.  In the end we sum all of those outputs.
