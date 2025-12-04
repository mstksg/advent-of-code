For these I have a handy utility function to parse an ascii map into a set of
points:

```haskell
data V2 a = V2 a a

parseAsciiSet :: (Char -> Bool) -> String -> Set (V2 Int)
```

and also a handy function that gets all eight neighbors of a point:

```haskell
fullNeighbsSet :: V2 Int -> Set (V2 Int)
```

They're actually fun to implement, exercise left to reader :)

Anyway once you have those, you can write a function of all reachable rolls:

```haskell
reachable :: Set (V2 Int) -> Set (V2 Int)
reachable pts = flip S.filter pts \pt ->
  S.size (fullNeighbsSet pt `S.intersection` pts) < 4
```

And so we have:

```haskell
part1 :: Set (V2 Int) -> Int
part1 = S.size . reachable

part2 :: Set (V2 Int) -> Int
part2 = S.size . fold . takeWhile (not . S.null) . unfoldr (Just . go)
  where
    go pts = (removed, pts `S.difference` removed)
      where
        removed = reachable pts
```
