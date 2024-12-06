This one features a common staple of Advent of Code: the 2D grid. In this case
we can parse it as a `Set Point` of boulders and an initial starting `Point`,
with `type Point = V2 Int` from the *linear* library, which has good `Num`,
`Functor`, `Foldable` instances etc.

Then the (possibly infinite) stepping function becomes:

```haskell
import Data.Finite
import Linear.V2
import qualified Data.Set as S
import qualified Data.Vector.Sized as SV

type Point = V2 Int

stepInDir :: Finite 4 -> Point
stepInDir = SV.index $ SV.fromTuple (V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0)

stepPath :: Int -> S.Set Point -> Point -> [(Point, Finite 4)]
stepPath maxCoord boulders = takeWhile inBounds . iterate go . (,0)
  where
    go (x, d)
      | x' `S.member` boulders = (x, d + 1)
      | otherwise = (x', d)
      where
        x' = x + stepInDir d
    inBounds = all (inRange (0, maxCoord))

part1 :: Set Point -> Point -> Int
part1 boulders = S.size . S.fromList . map fst . stepPath maxCoord boulders
  where
    maxCoord = maximum (foldMap toList boulders)
```

Here I use `Finite 4` to give a cyclic type I can repeatedly rotate, and look
up a single step in that direction from 4-vector. In my actual code I use a
data type `data Dir = North | East | South | West` that is essentially the same
thing.

For part 2 we can just try to insert new boulders along the original route and
count the boulders that give loops. We can use tortoise and hare to do loop
detection.

```haskell
hasLoop :: Eq a => [a] -> Bool
hasLoop xs0 = go xs0 (drop 1 xs0)
  where
    go (x:xs) (y:_:ys) = x == y || go xs ys
    go _ _ = False

part2 :: Set Point -> Point -> Int
part2 boulders p0 = length . filter goodBoulder . nubOrd $ stepPath maxCoord boulders
  where
    maxCoord = maximum (foldMap toList boulders)
    goodBoulder p = p /= p0 && hasLoop (stepPath maxCoord (S.insert p boulders) p)
```

Overall runs in about 1 second on my machine. You could optimize it a bit by
jumping directly to the next boulder. Basically you'd keep a map of x to the
y's of all boulders in that column so you can move vertically, and then a map
of y to the x's of all boulders in that row so you can move horizontally.

```haskell
collapseAxes :: Foldable f => f Point -> V2 (Map Int (Set Int))
collapseAxes = foldl' (flip addAxesMap) mempty

addAxesMap :: Point -> V2 (Map Int (Set Int)) -> V2 (Map Int (Set Int))
addAxesMap (V2 x y) (V2 xMaps yMaps) =
  V2
    (M.insertWith (<>) x (S.singleton y) xMaps)
    (M.insertWith (<>) y (S.singleton x) yMaps)

slideAxes :: V2 (Map Int (Set Int)) -> Point -> Finite 4 -> Maybe Point
slideAxes (V2 xMap yMap) (V2 x y) = SV.index $ SV.fromTuple
  ( S.lookupLT y (M.findWithDefault mempty x xMap) <&> \y' -> V2 x (y' + 1)
  , S.lookupGT x (M.findWithDefault mempty y yMap) <&> \x' -> V2 (x' - 1) y
  , S.lookupGT y (M.findWithDefault mempty x xMap) <&> \y' -> V2 x (y' - 1)
  , S.lookupLT x (M.findWithDefault mempty y yMap) <&> \x' -> V2 (x' + 1) y
  )

stepPath' :: V2 (Map Int (Set Int)) -> Point -> [(Point, Finite 4)]
stepPath' as = unfoldr go . (,0)
  where
    go (p, d) = do 
      p' <- slideAxes as p d
      pure ((p', d + 1), (p', d + 1))

part2' :: Set Point -> Point -> Int
part2' boulders p0 = length . filter goodBoulder . nubOrd $ stepPath maxCoord boulders
  where
    maxCoord = maximum (foldMap toList boulders)
    axesMap0 = collapseAxes boulders
    goodBoulder p = p /= p0 && hasLoop (stepPath' (addAxesMap p axesMap0) p)
```
