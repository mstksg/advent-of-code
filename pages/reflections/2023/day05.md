Another year and another puzzle where I feel like using Haskell's
[data-interval][] is cheating :)  It lets us work with ranged intervals and
gives us types like `IntervalSet` (a set of intervals) and `IntervalMap`
(mapping intervals to values), but with the correct implementations of map
intersection, set intersection, etc. that is aware of how intervals work.  We 
can generate a single `Interval`:

[data-interval]: https://hackage.haskell.org/package/data-interval

```haskell
import Data.Interval (Interval)
import qualified Data.Interval as IV

fromRange :: Int -> Int -> Interval Int
fromRange x len = IV.Finite x IV.<=..< IV.Finite (x + len)
```

The main type we will be using here is the `IntervalMap` type.  We will use it 
to map intervals to deltas: if you fall inside the interval, your value will 
update by `+ delta`.

```haskell
import Data.IntervalMap.Strict (IntervalMap)
import qualified Data.IntervalMap.Strict as IVM

-- | Takes in the (a, b, c) triples that the problem gives map chunks
buildMap :: [(Int, Int, Int)] -> IntervalMap Int Int
buildMap = IVM.fromList
         . map (\(dest, src, len) -> fromRange src len, dest - src)
```

Now we can run it on a single point:

```haskell
convertSingle :: Int -> IntervalMap Int Int -> Int
convertSingle x mp = case IVM.lookup x mp of
  Nothing    -> x           -- the value is not in any known interval
  Just delta -> x + delta   -- that interval has the given delta
```

So part 1 is simply finding the minimum after iterating `convertSingle` across 
every range:

```haskell
day05a :: [IntervalMap Int Int] -> [Int] -> Int
day05a imaps = minimum . map (\s0 -> foldl' convertSingle s0 imaps)
```

Part 2 is where it gets interesting.  We actually want to keep track of an 
`IntervalSet` -- the set of all intervals that we have seeds at.  We need to 
write a function `convertMany` that takes an `IntervalSet` and runs that 
interval set through an `IntervalMap` appropriately.

To do this, we find all of the "misses" (the intervals in the `IntervalSet` 
that aren't mapped) and the "hits" (the intervals in the `IntervalSet` that do 
exist in the map, shifted by the delta) and then recombine it:

```haskell
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IVS

convertMany :: IntervalMap Int Int -> IntervalSet Int -> IntervalSet Int
convertMany mp xs = misses <> hits
  where
    -- dummy map corresponding to putting `()` at every interval in our
    -- `IntervalSet`, needed because interval map functions require maps
    tempMap :: IntervalMap Int ()
    tempMap = IVM.fromList . map (,()) . IVS.toList $ xs

    misses = IVM.keysSet $ tempMap `IVM.difference` mp
    hits =
      IVS.fromList
        . map (\(iv, delta) -> IV.mapMonotonic (+ delta) iv)
        . IVM.toList
        $ IVM.intersectionWith const mp tempMap
```

And run it all together with:

```haskell
day05b :: IntervalSet Int -> [IntervalMap Int Int] -> IntervalSet Int
day05b = foldl' convertMany
```
