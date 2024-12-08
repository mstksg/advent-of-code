Mostly straightforward Haskell, building up the set of all antinodes by
iterating over every pair of antennae.  The main thing we parameterize over is
the way of generating the antinode points from a given pair of locations.

```haskell
makeAntinodes :: Eq a => Map Point a -> (Point -> Point -> [Point]) -> Set Point
makeAntinodes mp genPts = S.fromList do
  (p1, c1) <- M.toList mp
  (p2, c2) <- M.toList mp
  guard $ p1 /= p2 && c1 == c2
  genPts p1 p2

day08 :: (Point -> Point -> [Point]) -> NEMap Point (Maybe Char) :~> Int
day08 stepper mp = S.size $
    makeAntinodes ants \p1 p2 ->
      takeWhile (inBoundingBox bb) $ stepper p1 p2
  where
    bb = boundingBox (NEM.keys mp)
    ants = NEM.mapMaybe id mp

day08a :: NEMap Point (Maybe Char) -> Int
day08a = day08 \p1 p2 -> [p2 + p2 - p1]

day08b :: NEMap Point (Maybe Char) -> Int
day08b = day08 \p1 p2 -> iterate (+ (p2 - p1)) p2
```

Using some utility functions I have laying around:

```haskell
boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> V2 (g a)
boundingBox =
  (\(T2 (Ap mn) (Ap mx)) -> V2 (getMin <$> mn) (getMax <$> mx))
    . foldMap1 (\p -> T2 (Ap (Min <$> p)) (Ap (Max <$> p)))

inBoundingBox :: (Applicative g, Foldable g, Ord a) => V2 (g a) -> g a -> Bool
inBoundingBox (V2 mn mx) x = and $ go <$> x <*> mn <*> mx
  where
    go x' mn' mx' = x' >= mn' && x' <= mx'
```
