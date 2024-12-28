Problems like this showcase the utility of using `V2` from *linear* for keeping
track of points. The "step" function ends up pretty clean:

```haskell
type Point = V2 Int

step :: Point -> Point -> Point
step v x = mod <$> (x + v) <*> V2 101 103
```

Also, if we parse into `[V2 Point]` (a position and velocity paired up in a
`V2`) we can use `sequence` to unzip our list into a `V2 [Point] [Point]`, a
list of positions and velocities. We can then use `iterate` and `zipWith` to
step them:

```haskell
part1 :: [V2 Point] -> Int
part2 pvs = score $ iterate (zipWith step vs) ps !! 100
  where
    V2 ps vs = sequence pvs
    score = product . M.fromListWith (+) . mapMaybe (\p -> (classify p, 1))
    quadrant p = mfilter (notElem EQ) $ Just (compare <$> p <*> V2 50 51)
```

`quadrant` here uses the `Applicative` instance and also the `Foldable`
instance with `notElem`.

For my original solve of part 2, i stopped when I detected any large clusters.
But, once we see that the actual input consists of vertical and horizontal
lines, we can do a bit of optimizations. We know that the x positions have a
period of 101, and so frames with vertical lines appear with period 101.  We
know that y positions have a period of 103 and so frames with horizontal lines
appear with period 103. So, we can look at the first 101 frames and find any
vertical lines, and then the first 103 frames and find any horizontal lines,
and then do some math to figure out when the periodic appearances will line up.

```haskell
maxMargin :: [[Int]] -> Int
maxMargin = fst . maximumBy (comparing (concentration . snd)) . zip [0..]
  where
    concentration = product . M.fromListWith (+) . map (,1)

part1 :: [V2 Point] -> Int
part2 pvs = (xi + ((yi - xi) * 5151)) `mod` 10403
  where
    V2 ps vs = sequence pvs
    steps = iterate (zipWith step vs) ps
    xi = maxMargin (view _x <$> take 101 steps)
    yi = maxMargin (view _y <$> take 103 steps)
```
