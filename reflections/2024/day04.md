Here we are matching "stencils" across different windows, so it's always fun
to use comonads for this. That's because
`extend :: (w a -> b) -> w a -> w b` lets you automagically convert a function
on windows (the `w a -> b`) to a `w a -> w b`, the application across every
window.

First we parse our input into a `Map Point Char`, where `data V2 a = V2 a a`
(from the *linear* library), a tuple type with the correct `Num` instance that
I use for most of these.

Our stencils are (centered around 0,0):

```haskell
xmas :: [Map (V2 Int) Char]
xmas =
    [ M.fromList [(i *^ step, x) | (i, x) <- zip [0 ..] "XMAS"]
    | d <- [V2 1 0, V2 0 1, V2 1 1, V2 (-1) 1]
    , step <- [d, negate d]

    ]

crossMas :: [Map (V2 Int) Char]
crossMas = map (M.insert 0 'A') $ M.union <$> diag1 <*> diag2
  where
    diag1 = M.fromList . zip [V2 (-1) (-1), V2 1 1] <$> ["MS", "SM"]
    diag2 = M.fromList . zip [V2 1 (-1), V2 (-1) 1] <$> ["MS", "SM"]
```

Now some utility functions to wrap and unwrap our `Map (V2 Int) Char` into a
`Store (V2 Int) (Maybe Char)` store comonad, so we can use its Comonad
instance:

```haskell
mapToStore :: (Ord k, Num k) => Map k a -> Store k (Maybe a)
mapToStore mp = store (`M.lookup` mp) 0

mapFromStore :: Num k => Set k -> Store k a -> Map k a
mapFromStore ks = experiment \x -> M.fromSet (+ x) ks
```

Now a function to check if a stencil matches a neighborhood:

```haskell
checkStencil :: Num k => Map k a -> Store k (Maybe a) -> Bool
checkStencil mp x = all (\(p, expected) -> peeks (+ p) x == Just expected) (M.toList mp)

countWindowMatches :: (Num k, Eq a) => [Map k a] -> Store k (Maybe a) -> Int
countWindowMatches mps x = length $ filter (`matchMap` x) mps
```

Now we have a `Store k (Maybe a) -> Int`, which takes a window and gives an `Int` that
is the number of stencil matches at the window origin.  The magic of comonad
is that now we have `extend stencils :: Store k (Maybe a) -> Store k Int`,
which runs that windowed function across the entire map.

```haskell
countMatches :: [Map (V2 Int) a] -> Map (V2 Int) Char -> Int
countMatches stencils xs =
    sum . mapFromStore (M.keysSet xs) . extend (matchAnyMap stencils) . mapToStore $ xs

part1 :: Map (V2 Int) Char -> Int
part1 = countMatches xmas

part2 :: Map (V2 Int) Char -> Int
part2 = countMatches crossMas
```
