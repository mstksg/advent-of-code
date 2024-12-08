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

day08 :: (Point -> Point -> [Point]) -> Map Point (Maybe Char) :~> Int
day08 stepper mp = S.size $
    makeAntinodes ants \p1 p2 ->
      takeWhile (`S.member` allPoints) $ stepper p1 p2
  where
    allPoints = boundingBox (M.keysSet mp)
    ants = M.mapMaybe id mp

day08a :: Map Point (Maybe Char) -> Int
day08a = day08 \p1 p2 -> [p2 + p2 - p1]

day08b :: Map Point (Maybe Char) -> Int
day08b = day08 \p1 p2 -> iterate (+ (p2 - p1)) p2
```
