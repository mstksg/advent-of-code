This is puzzle involves iteratively following "steps" and seeing how things
change.  If we store the world state polymorphically as a `Map Point a`, then
we can write something generic to unite both parts.

Our polymorphic stepper will take a:

1.  `Set Point` of immovable walls
2.  A "glue" function `Point -> Dir -> a -> [(Point, a)]` which takes an `a`
    world entity and return any other entity it will be glued to.
2.  A starting state `(Point, Map Point a)`, the player position and the
    position of the crates
3.  A `Dir` motion

and return the new updated `(Point, Map Point a)` state.

It will work by first trying to update the person state: if it moves into a
crate, try to move the crate in the same direction, `Point -> Map Point a -> a
-> Maybe (Map Point a)`. This will then recursively try to move any crates
along the way and any crates glued to it. The whole thing is wrapped up in a
big `Maybe` monad, sequenced together with `foldlM`, so if anything fails, the
whole thing fails.

```haskell
type Point = V2 Int
data Dir = North | East | South | West

moveByDir :: Point -> Dir -> Point
moveByDir p d = p + case d of
  North -> V2 0 1
  East -> V2 1 0
  South -> V2 0 (-1)
  West -> V2 (-1) 1

stepper ::
  forall a.
  (Point -> Dir -> a -> [(Point, a)]) ->
  Set Point ->
  (Point, Map Point a) ->
  Dir ->
  (Point, Map Point a)
stepper glue walls (person, crates) d
  | person' `S.member` walls = (person, crates)
  | otherwise = case M.lookup person' crates of
      Just lr -> maybe (person, crates) (person',) $ tryMove person' crates lr
      Nothing -> (person', crates)
  where
    person' = person `moveByDir` d
    tryMove :: Point -> Map Point a -> a -> Maybe (Map Point a)
    tryMove p crates' moved = do
      foldlM (\cs (p', moved') -> tryMoveSingle p' cs moved') crates' ((p, moved) : glue p d moved)
    tryMoveSingle :: Point -> Map Point a -> a -> Maybe (Map Point a)
    tryMoveSingle p crates' moved =
      commit
        <$> if p' `S.member` walls
          then Nothing
          else case M.lookup p' crates' of
            Just lr -> tryMove p' crates' lr
            Nothing -> Just crates'
      where
        p' = p `moveByDir` d
        commit = M.delete p . M.insert p' moved
```

Now to pick the glue and the `a`: for part 1, each crate contains no extra
information, so `a` will be `()` and `glue _ _ _ = []`, no glue.

```haskell
part1 :: Set Point -> Set Point -> Point -> [Dir] -> Set Point
part1 crates walls person =
    M.keys . snd . foldl' (stepper glue crates) (person, M.fromSet (const ()) walls)
  where
    glue _ _ _ = []
```

For part 2, each crate is either a `[` or a `]`, left or right. So we can have
the `a` be `Bool`, and the glue being the corresponding pair, but only if the
motion direction is vertical.

```haskell
part2 :: Set Point -> Map Point Bool -> Point -> [Dir] -> Set Point
part2 crates walls person =
    M.keys . snd . foldl' (stepper glue crates) (person, walls)
  where
    glue p d lr = [(bump lr p, not lr) | d `elem` [North, South]]
    bump = \case
      False -> (+ V2 1 0)
      True -> subtract (V2 1 0)
```

We can score our set of points:

```haskell
score :: Set Point -> Int
score = sum . map (\(V2 x y) -> 100 * y + x) . toList 
```

