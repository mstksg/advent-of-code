Day 1's are usually pretty good for Haskell's stream processing, and this is no
exception :)

To get the list of elf calories, we split on the double newlines, and take the
sum of the lines of each group.

```haskell
import Data.List.Split (splitOn)

getCalories :: String -> [Int]
getCalories = map (sum . map read . lines) . splitOn "\n\n"
```

For part 1, this involves just finding the maximum.

```haskell
part1 :: String -> Int
part1 = maximum . getCalories
```

For part 2, we find the sum of the top 3 values:

```haskell
part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . getCalories
```
