Both of these can be done with knot-tying -- part 1 tying the knot upwards from
bottom to top to check if a given splitter received any light, and part 2 tying
the knot downwards to add up the number of paths under both paths of a
splitter. Assuming you have your splitter as a `Set Point`:

```haskell
type Point = V2 Int

part1 :: Point -> Set Point -> Int
part1 sourcePos splitters = M.size $ M.filter id reached
  where
    reached :: Map Point Bool
    reached = flip M.fromSet splitters \(V2 x y0) ->
      let cands = takeWhile ((`S.notMember` splitters) . V2 x) [y0 - 2, y0 - 4 .. 0]
       in flip any cands \y ->
            V2 x y == sourcePos
              || NEM.findWithDefault False (V2 (x - 1) y) reached
              || NEM.findWithDefault False (V2 (x + 1) y) reached
```

`cands` climbs all the way up until it hits a splitter, which effectively
blocks the beam. So, along the way, we either hit the source (`V2 x y ==
sourcePos`) or hit the output of a splitter, in which case our result is if
that splitter itself got any light.

```haskell
part2 :: Point -> Set Point -> Int
part2 sourcePos splitters = pathsFrom M.! (sourcePos + V2 0 2)
  where
    maxY = maximum $ S.map (view _y) $ map splitters
    downFrom (V2 x y0) = fromMaybe 1 $ listToMaybe 
      [ n
      | Just n <- M.lookup . V2 x <$> [y0, y0 + 2 .. maxY]
      ]
    pathsFrom :: Map Point Int
    pathsFrom = flip M.fromSet splitters \p ->
      downFrom (p + V2 1 2) + downFrom (p + V2 (-1) 2)
```

`downFrom` drops downwards until we leave the map (returning 1), or until we
hit a splitter, in which case the number of paths is the number of paths from
the splitter we hit.

Overall by only memoizing/knot-tying against the splitter set, we save a lot of
time from constructing the memo table for the entire grid.

I really want to unite both of these into a single function and swap out `+`
for `||` somehow, but I can't quite seem to massage them...
