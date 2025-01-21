Let's make a nice flexible `Gate` Functor/Traversable that will guide us along
our journey.

```haskell
data Op = OAnd | OOr | OXor
  deriving stock (Eq, Ord, Show, Generic)

data Gate a = Gate {gOp :: Op, gX :: a, gY :: a}
  deriving stock (Show, Generic, Functor, Traversable, Foldable)

applyGate :: Gate Bool -> Bool
applyGate Gate{..} = case gOp of
  OAnd -> gX && gY
  OOr -> gX || gY
  OXor -> gX /= gY
```

Part 1 we can use the typical knot-tying trick: from a `Map String (Gate
String)`, generate a `Map String Bool` of labels to their results, by referring
to that same result.  We use the `Functor` instance of `Gate` to get `fmap (M.!
result) :: Gate String -> Gate Bool`.

```haskell
part1 :: Map String Bool -> Map String (Gate String) -> Int
part1 inputs gates = sum [ 2 ^ read n | ('z':n, True) <- M.toList result ]
  where
    result :: Map String Bool
    result = inputs <> fmap (applyGate . fmap (M.! result)) gates
```

Now part 2, the fun part. One thing we can do is generate a full adder, by
creating a _tree_ of `Gate`s. We can use `Free` to create a tree of nested
Gates, since `Free f a = Pure a | Free (f (Free f a))`.

```haskell
type GateTree = Free Gate

halfAdder :: GateTree a -> GateTree a -> (GateTree a, GateTree a)
halfAdder x y = (wrap $ Gate OAnd x y, wrap $ Gate OXor x y)

-- | returns carry bit and output bit
fullAdder :: GateTree a -> GateTree a -> GateTree a -> (GateTree a, GateTree a)
fullAdder x y carry0 = (wrap $ Gate OOr carry1 carry2, o)
  where
    (carry1, z) = halfAdder x y
    (carry2, o) = halfAdder z carry0

-- | returns final carry bit and all n output bits
adderTree :: Int -> (GateTree String, NonEmpty (GateTree String))
adderTree n
  | n == 0 = (:| []) `second` halfAdder (pure "x00") (pure "y00")
  | otherwise =
      let (carryIn, rest) = adderTree (n - 1)
          (carryOut, new) = fullAdder (pure (printf "x%02d" n)) (pure (printf "y%02d" n)) carryIn
       in (carryOut, new `NE.cons` rest)
```

Now for the magic of `Free`: We can collapse it all into a flattened free
structure using `iterA`, which "folds" each layer of the free structure. We
built up a map of known gates and assign unknown gates to a new unique ID,
creating a `Map (Gate (Either Int String)) Int`. `Left` means that the gate
points to a known `Int` id and `Right` means it was an input `xNN`/`yNN`
variable.

```haskell
unrollGates ::
  forall a. Ord a => GateTree a -> State (Int, Map (Gate (Either Int a)) Int) (Either Int a)
unrollGates = iterA go . fmap Right
  where
    go g0 = do
      gate <- sequenceA g0
      (currIx, currMp) <- get
      case M.lookup gate currMp of
        Nothing -> do
          put (currIx + 1, M.insert gate currIx currMp)
          pure $ Left currIx
        Just i -> pure $ Left i

unrollAdderTree :: Int -> ([Int], IntMap (Gate (Either Int String)))
unrollAdderTree n = (lefts $ toList outs, IM.fromList $ swap <$> M.toList mp)
  where
    (carry, adder) = adderTree n
    full = NE.reverse $ carry `NE.cons` adder
    (outs, (_, mp)) = runState (traverse unrollGates full) (0, M.empty)
```

We wrapped it all up with `unrollAdderTree`, which returns the map of gate Int
id's and also all of the top-level output id's. This works because all of the
adders in `carry`/`adder`/`full` are the top-level outputs, so `traverse` pulls
out those `Int`s as its final result.

Finally we can wrap it all up in the list monad for a search. The whole thing
is composing `NameState -> [NameState]` branches using `>=>`, where dead-ends
are indicated by an empty list returned.

```haskell
data NameState = NS
  { nsRenames :: Map String String
  , nsNames :: IntMap String
  , nsFound :: Bool
  }

nameGate :: Map (Gate String) String -> Int -> Gate (Either Int String) -> NameState -> [NameState]
nameGate avail ng g0 NS{..} =
  case applySwaps nsRenames <$> M.lookup gate avail of
    Nothing -> []
    Just here ->
      -- the all-goes-well branch
      NS{nsNames = IM.insert ng here nsNames, ..}
      -- all possible substitutions/switches
        : [ NS renames (IM.insert ng there nsNames) True
          | not nsFound
          , there <- toList avail
          , here /= there
          , let renames = M.fromList [(here, there), (there, here)] <> nsRenames
          ]
  where
    gate = either (nsNames IM.!) id <$> g0
    applySwaps mp x = M.findWithDefault x x mp

nameTree :: Map (Gate String) String -> [Map String String]
nameTree avail = nsRenames <$> foldr (\o -> (go o >=>)) pure outGates s0
  where
    s0 = NS M.empty IM.empty False
    (outGates, gates) = unrollAdderTree 44
    go outGate ns0
      | M.size (nsRenames ns0) == 8 = [ns0]
      | otherwise =
          IM.foldrWithKey
            (\k g -> (nameGate avail k g >=>))
            pure
            (IM.takeWhileAntitone (<= outGate) gates)
            (ns0{nsFound = False})
```

The search is meant layer-by-layer: do all of the `z00` inputs first, then the
`z01` inputs, etc.  There is also a major optimization that makes this all
feasible: we only expect one swap per layer.

Anyway that's it:

```haskell
part2 :: Map (Gate String) String -> [String]
part2 = fmap M.keys . listToMaybe . nameTree
```
