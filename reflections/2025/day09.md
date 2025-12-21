
Assuming we have our points as a list of points (`[V2 Int]`), the "area"
between two points is

```haskell
rectArea :: V2 Int -> V2 Int -> Int
rectArea p q = product $ zipWith (+) (toList p) (toList q)
```

And so part 1 is pretty straightforward to brute force:

```haskell
part1 :: [V2 Int] -> Int
part1 pts = maximum
    [ rectArea p q
    | p : ps <- tails pts
    , q <- ps
    ]
```

For part 2 it's basically the same, except we need to make sure each rectangle
is legal. For that we can use the
[data-interval](https://hackage.haskell.org/package/data-interval) library,
which has come in clutch before in this same year. We can assume we have an
`IntervalMap Int (IntervalSet Int)`, which maps each x-range to the y-ranges
that are legal.  For example, in the example set, we have:

```
 01234567890123
0..............
1.......#XXX#..
2.......XXXXX..
3..#XXXX#XXXX..
4..XXXXXXXXXX..
5..#XXXXXX#XX..
6.........XXX..
7.........#X#..
9..............
```

For the x range `2..6` we have the y range `3..5` filled, for the x range
`7..8` we have y range `1..5` filled, for the x range `9..11` we have the
y range `1..7` filled. Assuming we had this legal bounds, `part2` is the same
but we make sure the box area subtracted by the legal bounds area is empty:

```haskell
part2 :: [V2 Int] -> Int
part2 pts = maximum
    [ rectArea p q
    | p@(V2 px py) : ps <- tails pts
    , q@(V2 qx qy) <- ps
    , let xRange = Finite (min px qx) <=..< Finite (max px qx)
          yRange = Finite (min py qy) <=..< Finite (max py qy)
          region = IVM.singleton xRange (IVS.singleton yRange)
          outOfBounds = IVM.intersectionWith IVS.difference region allowedRegion
    , all IVS.null outOfBounds
    ]
  where
    allowedRegion = regions pts
```

Now the fun part is to build up `regions :: [V2 Int] -> IntervalMap Int
(InteveralSet Int)`.  I did it by using a `scanl`: first, make a list `[(Int,
Set Int)]` of `[(x, [y0,y1,y2]), ...]`, all of the x values of all the points,
paired with the y's of those points. Then, we can `scanl` over that list to
produce all of the x scan lines: at each `x`, add out the y's that just
appeared, and close out the y's that were already seen. That gives us our
entire scan.

```haskell
-- | x coords to the y coordinates they cointain
regions :: [V2 Int] -> IntervalMap Int (IntervalSet Int)
regions pts = IVM.fromList $ zip (drop 1 xRanges) yRanges
  where
    -- the (sorted) x's of all points, with the y's they are at.
    xs :: [(Int, Set Int)]
    xs =
      M.toList $
        M.fromListWith
          (<>)
          [ (x, S.singleton y)
          | V2 x y <- pts
          ]
    xRanges :: [Interval Int]
    yRanges :: [IntervalSet Int]
    (xRanges, yRanges) = unzip $ scanl' go (NegInf <..< Finite 0, IVS.empty) xs
      where
        go (i0, curr) (x, ys) = (IV.upperBound i0 <=..< Finite x, curr')
          where
            curr' = (curr `IVS.union` ivs) `IVS.difference` (curr `IVS.intersection` ivs)
            ivs =
              IVS.fromList
                [ Finite a <=..< Finite b
                | [a, b] <- chunksOf 2 (S.toList ys)
                ]
```
