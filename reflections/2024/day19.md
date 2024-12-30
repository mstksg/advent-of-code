This one can be solved using an infinite trie --- we build up an infinite trie
of possibilities using patterns, and then look up a given design by tearing
down that trie. Written altogether that gives us a hylomorphism! I've written
about using tries with recursion schemes [in my
blog](https://blog.jle.im/entry/tries-with-recursion-schemes.html), so this
seemed like a natural extension.

```haskell
data CharTrie a = CT {ctHere :: Maybe a, ctThere :: IntMap (CharTrie a)}
  deriving stock (Show, Functor, Traversable, Foldable)

makeBaseFunctor ''CharTrie

-- generates for us:
data CharTrieF a r = CTF {ctHereF :: Maybe a, ctThereF :: Map Char r}
  deriving stock (Show, Functor, Traversable, Foldable)
```

We can parameterize on a monoid `a` to solve both parts. For part 1, `a` is
`()`: `Just ()` means that the design is in the trie, and `Nothing` means it is
not.  For part 2, `a` is `Sum Int`: `Just (Sum n)` means there are `n` ways to get
this design, and `Nothing` means the design is unreachable.

First, the lookup algebra, which is standard for tries:

```haskell
lookupAlg :: CharTrieF a (String -> Maybe a) -> String -> Maybe a
lookupAlg CTF{..} = \case
  [] -> ctHereF
  c : cs -> ($ cs) =<< M.lookup c ctThereF
```

If we had a `CharTrie a`, then `cata lookupAlg myTree "hello"` would look up
`"hello"` in the trie.

The buildup co-algebra is an interesting one.  We will convert a `Map String a`
into a `CharTrie a`, _but_, every time we reach the end of the string, we
"restart" the building from the start. So, we'll take a `Set String` as well,
which we will trigger when we hit the end of a pattern.

```haskell
fromMapCoalg ::
  forall a.
  (Semigroup a) =>
  Set String ->
  Map String a ->
  CharTrieF a (Map String a)
fromMapCoalg mp0 = \ks ->
  let x = M.lookup [] ks
      reAdd = case x of
        Nothing -> id
        Just y -> M.unionWith (M.unionWith (<>)) (M.fromSet (const y) <$> initialSplit)
   in CTF x $ reAdd (splitTrie ks)
  where
    initialSplit :: Map Char (Set String)
    initialSplit = M.fromAscListWith (<>) [ (k, S.singleton ks) | k : ks <- toList mp0 ]
    splitTrie :: Map String a -> Map Char (Map String a)
    splitTrie mp = M.fromAscListWith (<>) [ (k, M.singleton ks x) | (k : ks, x) <- M.toList mp ]
```

And that's it! Our hylomorphism will build up the infinite trie, but _only_ the
specific branch that we end up looking up from it. Because it's a hylomorphism,
we never actually generate any trie structure: we basically build up only the
branch we care about (driven by the lookup) and stop when we finish looking up
or hit a dead end.

````haskell
buildable :: (Semigroup a) => a -> Set String -> String -> Maybe a
buildable x mp = hylo lookupAlg (fromMapCoalg mp) (M.fromSet (const x) mp)

part1 :: Set String -> [String] -> Int
part1 pats = length . mapMaybe (buildable () pats)

part2 :: Set String -> [String] -> Int
part2 pats = getSum . foldMap (fold . buildable (Sum 1) pats)
```
