Today's "one trick" seems to be realizing that the actual ordered "list" is a
red herring: a number's progression doesn't depend on any of its neighbors or
ordering. So what we really have is not a list, but a _multi-set_. Stepping the
multiset through 75 iterations is very efficient --- shows you what you gain
when you use the correct data structure to represent the state!

```haskell
freqs :: [Int] -> IntMap Int
freqs = IM.fromListWith (+) . map (,1)

stepMap :: IntMap Int -> IntMap Int
stepMap mp = IM.unionsWith (+)
  [ freqs (map (* n) (step x))
  | (x, n) <- IM.toList mp
  ]

step :: Int -> [Int]
step c
  | c == 0 = [1]
  | even pow = let (a, b) = c `divMod` (10 ^ (pow `div` 2))
                in [a, b]
  | otherwise = [c * 2024]
  where
    pow = numDigits c

part1 :: [Int] -> Int
part1 = sum . (!! 25) . iterate stepMap . freqs

part2 :: [Int] -> Int
part2 = sum . (!! 75) . iterate stepMap . freqs
```

My original reflections/write-up used data-memocombinators, but after some
thought I believe that the frequency map approach is the most natural.
