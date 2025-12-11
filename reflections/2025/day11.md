
This one yields itself very nicely to knot-tying: we create the map of the
ways to get to the `"out"` by being 1 if we are one away from `out`, or else
the sum of all of the ways of our next hops.  Because of knot-tying, this
handles the memoization for us using lazy maps.

```haskell
pathsTo :: Ord a => Map a [a] -> a -> a -> Int
pathsTo conns start end = res M.! start
  where
    res =
      conns <&> \nexts ->
        sum
          [ if y == end then 1 else M.findWithDefault 0 y res
          | y <- nexts
          ]

part1 :: [(String, [String])] -> Int
part1 conns = pathsTo (M.fromList conns) "you" "out"
```

Part 2 we can do the same thing, except our "state nodes" are now `(String, Set
String)` instead of `String`: we have the current path the set of "points we
must still visit". In this case, we start at node `("svr", S.fromList ["dac","fft])`
and end at node `("out", S.empty)`:

```haskell
-- | Keys are all of the original keys, but repeated for every subset: instead
-- of 'foo', we have (foo, [dac,fft]), (foo, [dac]), (foo, [fft]), and (foo,
-- []).
addVisited :: Ord a => Map a [a] -> Set a -> Map (a, Set a) [(a, Set a)]
addVisited conns required =
  M.fromList
    [ ((x, subset), map (,S.delete x subset) xs)
    | (x, xs) <- M.toList conns
    , subset <- toList $ S.powerSet required
    ]

part2 :: [(String, [String])] -> Int
part2 conns = pathsTo (M.fromList xs `addVisited` toVisit) ("svr", toVisit) ("out", S.empty)
  where
    toVisit = S.fromList ["dac", "fft"]
```

Creating `addVisited` we just need to make sure that if we descend from one of
the required items, that item is deleted from the set: that is, `(dac,
[dac,fff])` contains `[(dac,[fff])]`.
