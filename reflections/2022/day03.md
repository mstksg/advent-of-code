Some more "string" processing!  First, let's assume that we receive a list of
priority sequences instead of a list of character strings.  Both of these parts
are actually the same problem, the only difference is how we get our groups.
Once we get the groups, we can convert each string into an `IntSet` and find
the mutual intersection between all of the groups with `IS.intersection` and
`foldl1`:

```haskell
import qualified Data.IntSet as IS

solve :: [[Int]] -> Int
solve = sum . map go
  where
    getPriority = IS.findMin . foldl1 IS.intersection . map IS.fromList
```

Then each part is just finding the right splitting function:

```haskell
part1, part2 :: [Int] -> Int
part1 = solve . map (\xs -> chunksOf (length xs `div` 2) xs)
part2 = solve . chunksOf 3
```
