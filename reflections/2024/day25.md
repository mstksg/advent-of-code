As usual, a nice relaxing day to celebrate Christmas :)

Assuming we have a list of keys and locks interspersed, as `[Set (Int, Int)]`, we
can marginalize to get the x-wise histograms and y-wise histograms:

```haskell
marginX :: Set (Int, Int) -> Map Int Int
marginX = M.fromListWith (+) . map (\(x, y) -> (x, 1)) . toList

marginY :: Set (Int, Int) -> Map Int Int
marginY = M.fromListWith (+) . map (\(x, y) -> (y, 1)) . toList
```

We can distinguish keys from locks by checking if y=0 has all 5 points filled:

```haskell
isLock :: Set (Int, Int) -> Bool
isLock = (== 5) . M.findWithDefault 0 0 . marginY
```

We can check if a pair is valid by checking that none of their x margins add up
to greater than 7.  Wrapping it all in the list monad's cartesian product and
we get:

```haskell
day25 :: [Set (Int, Int)] -> Int
day25 = uncurry countCombos . partition isLock
  where
    countCombos locks keys = length do
      lock <- marginX <$> locks
      key <- marginX <$> keys
      guard $ all (< 8) (M.unionWith (+) lock key)
```
