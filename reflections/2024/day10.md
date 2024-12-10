A lot of times in Haskell, two problems end up having the same algorithm, just
with a different choice of `Monoid`.  This puzzle is a good example of that.

We can do a simple DFS and collect all 9's into a monoid:

```haskell
gatherNines :: Monoid m => (Point -> m) -> Map Point Int -> Point -> m
gatherNines f mp = go 0
  where
    go x p
      | x == 9 = f p
      | otherwise =
          foldMap (go (x+1)) . M.keys . M.filter (== (x+1)) $ mp `M.restrictKeys` neighbs
      where
        neighbs = S.fromList $ (p +) <$> [V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0]
```

For part 1 the monoid is `Set Point` (the unique 9's) and for part 2 the monoid
is `Sum Int` (number of paths)

```haskell
solve :: Monoid m => (Point -> m) -> (m -> Int) -> Map Point Int -> Int
solve gather observe mp =
    sum . map (observe . gatherNines gather mp) . M.keys $ M.filter (== 0) mp

part1 :: Map Point Int -> Int
part1 = solve S.singleton S.size

part2 :: Map Point Int -> Int
part2 = solve (const (Sum 1)) getSum
```
