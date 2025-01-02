Everything reveals itself if we imagine a lookup table of "best path from A to
B".  For my own purposes I've made the functions parameterized by button pad,
using `Maybe a`, where `Nothing` is the `A` key and `Just x` is the `x` key.

```haskell
type LookupTable a b = Map (Maybe a) (Map (Maybe a) [Maybe b])

type LookupTableLengths a = Map (Maybe a) (Map (Maybe a) Int)

toLengths :: LookupTable a b -> LookupTableLengths a
toLengths = fmap (fmap length)
```

The key is that now these maps are composable:

```haskell
spellDirPathLengths :: Ord a => LookupTableLengths a -> [Maybe a] -> Int
spellDirPathLengths mp xs = sum $ zipWith (\x y -> (mp M.! x) M.! y) xs (drop 1 xs)

composeDirPathLengths :: Ord b => LookupTableLengths b -> LookupTable a b -> LookupTableLengths a
composeDirPathLengths mp = (fmap . fmap) (spellDirPathLengths mp . (Nothing :))
```

That is, if you have the lookup table for two layers, you can compose them to
create one big lookup table.

```haskell
data Dir = North | East | West | South
data NumButton = Finite 10

dirPathChain :: [LookupTableLengths NumButton]
dirPathChain = iterate (`composeDirPathLengths` dirPath @Dir) (dirPathCosts @Dir)

solveCode :: Int -> [Maybe NumButton] -> Int
solveCode n = spellDirPathLengths mp . (Nothing :)
  where
    lengthChain = dirPathChain !! (n - 1)
    mp = lengthChain `composeDirPathLengths` dirPath @NumButton
````

The nice thing is that you only need to compute `dirPathChain` once, to get the
final `LookupTableLengths` for a given `n`, and you can re-use it for
everything.

Generating the actual `LookupTable NumButton Dir` and `LookupTable Dir Dir` is
the tricky part. For me I generated it based on the shortest path considering
the third bot up the chain from the bottom: I used an *fgl* graph where the
nodes were the state of three bots and the edges were the actions that the
fourth "controller" would take, and computed the shortest path in terms of the
fourth controller.  This seems to be the magic number: anything higher and you
get the same answer, anything lower and you get suboptimal final paths.
