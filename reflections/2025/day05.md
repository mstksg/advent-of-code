Maybe unfortunately, this one is just a straightforward application of the
*[data-interval](https://hackage.haskell.org/package/data-interval)* library.

The main data structure is an interval set:

```haskell
buildSet :: [(Int, Int)] -> IntervalSet Int
buildSet = foldMap \(x, y) -> IVS.singleton (Finite x I.<=..<= Finite y)
```

Part 1 is just filtering for the points in the set:

```haskell
part1 :: IntervalSet Int -> [Int] -> Int
part1 iset = length . filter (`IVS.member` iset)
```

And part 2 is just getting the size of that set:

```haskell
part2 :: IntervalSet Int -> Int
part2 = sum . map ((+ 1) . IV.width) . IVS.toAscList
```
