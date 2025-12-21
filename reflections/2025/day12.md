Uhh I got spoiled on this one, that you just need to check that the area of the
region is big enough to fill the area of all of the blocks

```haskell
goodEnough
    :: [Int]   -- ^ size of each block
    -> V2 Int  -- ^ bounds
    -> [Int]   -- ^ how much of each block
    -> Bool
goodEnough blockSizes bounds ns = product bounds >= sum (zipWith (*) blockSizes ns)

day12
    :: [Set (V2 Int)]     -- ^ blocks
    -> [(V2 Int, [Int])]  -- ^ things to try out
    -> Int
day12 blocks = length . filter (uncurry (goodEnough (map S.size blocks)))
```
