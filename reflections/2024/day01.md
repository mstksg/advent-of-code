Day 1 is always a Haskell warmup :)

One nice way to get both lists is to parse `[(Int, Int)]` and use `unzip ::
[(a,b)] -> ([a], [b])]`, getting a list of pairs into a pair of lists.

Once we have our two `[Int]`s, part 1 is a zip:

```haskell
part1 :: [Int] -> [Int] -> Int
part1 xs ys = sum $ map abs (zipWith subtract xs ys)
```

Part 2 we can build a frequency map and then map a lookup:


```haskell
import qualified Data.Map as M

part2 :: [Int] -> [Int] -> Int
part2 xs ys = sum $ map (\x -> x * M.findWithDefault 0 x freqMap) xs
  where
    freqMap :: M.Map Int Int
    freqMap = M.fromListWith (+) (map (,1) ys)
```
