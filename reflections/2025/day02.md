You can do this nicely using the `IntSet` type in the *containers* library,
with `IS.fromRange :: (Int, Int) -> IntSet`. Then you can just turn the ranges
`IntSet`s and intersect them with the IntSet of all invalid IDs.

```haskell
-- | repDigits 3 567 = 567567567
repDigits :: Int -> Int -> Int
repDigits n = read . concat . replicate n . show

-- | All duplicated IDs up to 1e11
rep2 :: IntSet
rep2 = IS.fromAscList . takeWhile (< 1e11) . map (repDigits 2) $ [1 ..]

part1 :: [(Int, Int)] -> Int
part1 = IS.foldl' (+) 0 . foldMap (IS.intersection rep2 . IS.fromRange)
```

And you can union together `rep2`, `rep3`, etc. too:

```haskell
repN :: IntSet
repN = flip foldMap [2..11] $ \n ->
  IS.fromAscList . takeWhile (< 1e11) . map (repDigits n) $ [1 ..]

part2 :: [(Int, Int)] -> Int
part2 = IS.foldl' (+) 0 . foldMap (IS.intersection repN . IS.fromRange)
```
