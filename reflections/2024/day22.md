First let's set up the RNG step:

```haskell
step :: Int -> Int
step = prune . phase3 . prune . phase2 . prune . phase1
  where
    phase1 n = (n `shift` 6) `xor` n
    phase2 n = (n `shift` (-5)) `xor` n
    phase3 n = (n `shift` 11) `xor` n
    prune = (.&. 16777215)
```

Part 1 is just running and summing:


```haskell
part1 :: [Int] -> Int
part1 = sum . map ((!! 2000) . iterate)
```

Part 2 is a little more interesting. We want to make a map of 4-sequences to
the first price they would get. On a chain of iterations, we can iteratively
chomp on runs of 4:

```haskell
chompChomp :: [Int] -> [([Int], Int)]
chompChomp (a : b : c : d : e : fs) =
    ([da, db, dc, dd], e) : chompChomp (b : c : d : e : fs)
  where
    da = b - a
    db = c - b
    dc = d - c
    dd = e - d
chompChomp _ = []

priceForChain :: Int -> Map [Int] Int
priceForChain = M.fromListWith (const id) . chompChomp . take 2000 . map (`mod` 10) . iterate step
```

Then we can sum all of the sequence prices and get the maximum:

```haskell
part2 :: [Int] -> Int
part2 = maximum . M.elems . M.fromListWith (+) . map priceForChain
```

I'm not super happy with the fact that this takes 3 seconds (even after
optimizing to using `IntMap` on a base-19 encoding of the sequence). Switching
to a single mutable vector doing all of the summing (and a mutable vector for
every seed preventing double-adds) we bring it down to 800ms which still isn't
particularly ideal.
