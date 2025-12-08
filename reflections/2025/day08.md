I originally solved this using the *fgl* library, but hand-rolling things seem
to be much faster (down from 2 seconds to 100ms), so I switched over to a
`State` monad base implementation:

```haskell
-- | Chomp the next pair and merge their networks, emitting the pair
chomp :: Ord b => State (Set (Set b), [(b, b)]) (b, b)
chomp = state \(clusts, (p, q) : pts) -> do
    let Just pclust = find (p `S.member`) clusts
        ~(Just qclust) = find (q `S.member`) clusts
        clusts'
          | q `S.member` pclust -> clusts
          | otherwise = S.insert (pclust <> qclust) . S.delete pclust . S.delete qclust $ clusts
    in  ((p, q), (clusts', pts))

-- | L.qd from the linear library gives the squared equclidean distance, so we
-- don't have to worry about floating point
sortedPairs :: (Ord b, L.Metric f, Num b) => [f b] -> [(f b, f b)]
sortedPairs pts = sortOn (uncurry L.qd) [(p, q) | p : ps <- tails pts, q <- ps]
```

And for part 1 we just literally do `replicateM 1000` in `State`:

```haskell
part1 :: [V3 Int] -> Int
part1 pts = product . take 3 . sortOn Down . map S.size . S.toList $ chunks
  where
    (chunks, _) = execState (replicateM 1000 chomp) (S.fromList (S.singleton <$> pts), sortedPairs pts)
```

For part 2 we can do a check to see if this is the step that we complete the
network:

```haskell
part2 :: [V3 Int] -> Int
part2 pts = px * qx
  where
    go = do
      out <- chomp
      isOne <- gets $ (== 1) . S.size . fst
      if isOne
        then pure out
        else go
    (V3 px _ _, V3 qx _ _) = evalState go (S.fromList $ S.singleton <$> pts, sortedPairs pts)
```
