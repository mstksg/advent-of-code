Again a straightforward Haskell day. I have a utility function I use for a
bunch of these:

```haskell
countTrue :: (a -> Bool) -> [a] -> Int
countTrue p = length . filter p
```

So we can run `countTrue` over our list of `[Int]`.  The predicate is:


```haskell
import Data.Ix (inRange)

predicate :: [Int] -> Bool
predicate xs =
  all (inRange (1, 3)) diffies
    || all (inRange (1, 3) . negate) diffies
  where
    diffies = zipWith subtract xs (drop 1 xs)
```

It's a straightforward application of `countTrue predicate` for part 1. For
part 2, we can see if any of the possibilities match the predicate.

```haskell
part1 :: [[Int]] -> Int
part1 = countTrue predicate

part2 :: [[Int]] -> Int
part2 = countTrue \xs ->
  let possibilities = xs : zipWith (++) (inits xs) (tail (tails xs))
   in any predicate possibilities
```

`inits [1,2,3]` gives us `[]`, `[1]`, `[1,2]`, and `[1,2,3]`, and `tail (tails
xs)` gives us `[2,3]`, `[3]`, and `[]`.  So we can zip those up to get
`[2,3]`, `[1,3]`, and `[2,3]`.  We just need to make sure we add back in our
original `xs`.
