{-# OPTIONS_GHC -Wno-orphans #-}

module AOC.Common.Point (
  -- * Points
  Point,
  Point3,
  Point4,
  FinPoint,
  cardinalNeighbs,
  cardinalNeighbsSet,
  fullNeighbs,
  fullNeighbsSet,
  mannDist,
  mannNorm,
  mulPoint,
  lineTo,

  -- * Linear operations
  module L,

  -- * Directions
  Dir (..),
  parseDir,
  parseDir',
  dirPoint,
  dirPoint',
  rotPoint,
  rotFin,
  mulDir,
  allDir,
  allDirSet,

  -- * Orientations
  D8 (..),
  mulD8,
  orientPoint,
  orientFin,
  allD8,
  allD8Set,

  -- * 2D Maps
  memoPoint,
  contiguousRegions,
  contiguousShapes,
  contiguousShapesBy,
  shiftToZero,
  shiftToZero',
  parseAsciiMap,
  asciiGrid,
  parseAsciiSet,
  ScanPoint (..),
  displayAsciiMap,
  displayAsciiSet,
  parseLetters,
  parseLettersSafe,

  -- ** Bounding boxes
  boundingBox,
  boundingBox',
  inBoundingBox,
  fillBoundingBox,
  minCorner,
  minCorner',

  -- ** Axes map
  collapseAxes,
  addAxesMap,
  slideAxes,

  -- * Util
  centeredFinite,
)
where

import qualified Advent.OCR as OCR
import Control.DeepSeq
import qualified Control.Foldl as F
import Control.Lens
import Data.Char
import Data.Finitary
import Data.Finite
import Data.Finite.Internal
import Data.Foldable
import Data.Group
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Map.Lens
import qualified Data.Map.NonEmpty as NEM
import Data.Maybe
import Data.MemoCombinators (Memo)
import qualified Data.MemoCombinators as Memo
import Data.Monoid
import Data.Ord
import Data.Proxy
import Data.Ratio
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Set (Set)
import qualified Data.Set as S
import Data.Set.Lens
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import Data.Tuple.Strict
import GHC.Generics
import GHC.TypeNats
import Linear
import qualified Linear.Algebra as L
import qualified Linear.V0 as L
import qualified Linear.V1 as L
import qualified Linear.V2 as L
import qualified Linear.V3 as L
import qualified Linear.Vector as L

-- | 2D Coordinate
type Point = V2 Int

type Point3 = V3 Int

type Point4 = V4 Int

type FinPoint n = V2 (Finite n)

-- | Find the minimum and maximum x and y from a collection of points.
--
-- Returns @'V2' (V2 xMin yMin) (V2 xMax yMax)@.
boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> V2 (g a)
boundingBox =
  (\(T2 (Ap mn) (Ap mx)) -> V2 (getMin <$> mn) (getMax <$> mx))
    . foldMap1 (\p -> T2 (Ap (Min <$> p)) (Ap (Max <$> p)))

-- | A version of 'boundingBox' that works for normal possibly-empty lists.
boundingBox' :: (Foldable f, Applicative g, Ord a) => f (g a) -> Maybe (V2 (g a))
boundingBox' = fmap boundingBox . NE.nonEmpty . toList

fillBoundingBox ::
  (Foldable f, Applicative g, Ord a, Ord (g a), Traversable g, Enum a) =>
  f (g a) ->
  Set (g a)
fillBoundingBox ps = case boundingBox' ps of
  Nothing -> S.empty
  Just (V2 mins maxs) -> S.fromList $ sequenceA $ liftA2 enumFromTo mins maxs

minCorner :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> g a
minCorner = fmap getMin . getAp . foldMap1 (Ap . fmap Min)

minCorner' :: (Foldable f, Applicative g, Ord a) => f (g a) -> Maybe (g a)
minCorner' = fmap minCorner . NE.nonEmpty . toList

-- | Shift corner to (0,0)
shiftToZero ::
  (Applicative f, Num a, Ord a) =>
  NESet (f a) ->
  NESet (f a)
shiftToZero ps = NES.mapMonotonic (liftA2 subtract mn) ps
  where
    mn = minCorner ps

-- | Shift corner to (0,0)
shiftToZero' ::
  (Applicative f, Num a, Ord a) =>
  Set (f a) ->
  Set (f a)
shiftToZero' ps = case minCorner' ps of
  Nothing -> ps
  Just mn -> S.mapMonotonic (liftA2 subtract mn) ps

inBoundingBox :: (Applicative g, Foldable g, Ord a) => V2 (g a) -> g a -> Bool
inBoundingBox (V2 mn mx) x = and $ go <$> x <*> mn <*> mx
  where
    go x' mn' mx' = x' >= mn' && x' <= mx'

collapseAxes :: Foldable f => f Point -> V2 (Map Int (Set Int))
collapseAxes = foldl' (flip addAxesMap) mempty

addAxesMap :: Point -> V2 (Map Int (Set Int)) -> V2 (Map Int (Set Int))
addAxesMap (V2 x y) (V2 xMaps yMaps) =
  V2
    (M.insertWith (<>) x (S.singleton y) xMaps)
    (M.insertWith (<>) y (S.singleton x) yMaps)

slideAxes :: V2 (Map Int (Set Int)) -> Point -> Dir -> Maybe Point
slideAxes (V2 xMap yMap) (V2 x y) = \case
  North -> S.lookupGT y (M.findWithDefault mempty x xMap) <&> \y' -> V2 x (y' - 1)
  East -> S.lookupGT x (M.findWithDefault mempty y yMap) <&> \x' -> V2 (x' - 1) y
  South -> S.lookupLT y (M.findWithDefault mempty x xMap) <&> \y' -> V2 x (y' + 1)
  West -> S.lookupLT x (M.findWithDefault mempty y yMap) <&> \x' -> V2 (x' + 1) y

cardinalNeighbs :: Point -> [Point]
cardinalNeighbs p = (p +) <$> [V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0]

cardinalNeighbsSet :: Point -> Set Point
cardinalNeighbsSet p =
  S.fromDistinctAscList . map (p +) $
    [ V2 (-1) 0
    , V2 0 (-1)
    , V2 0 1
    , V2 1 0
    ]

fullNeighbs ::
  (Applicative f, Num a, Traversable f) =>
  f a ->
  [f a]
fullNeighbs p =
  drop
    1
    [ liftA2 (+) p d
    | d <- sequence (pure [0, -1, 1])
    ]
{-# INLINE fullNeighbs #-}

fullNeighbsSet ::
  (Applicative f, Num a, Ord (f a), Traversable f) =>
  f a ->
  Set (f a)
fullNeighbsSet p =
  S.fromDistinctAscList $
    [ liftA2 (+) p d
    | d <- sequence (pure [-1, 0, 1])
    , d /= pure 0
    ]

-- | Find contiguous regions by cardinal neighbors
contiguousRegions ::
  Set Point ->
  Set (NESet Point)
contiguousRegions = startNewPool S.empty
  where
    startNewPool seenPools remaining = case S.minView remaining of
      Nothing -> seenPools
      Just (x, xs) ->
        let (newPool, remaining') = fillUp (NES.singleton x) S.empty xs
         in startNewPool (S.insert newPool seenPools) remaining'
    fillUp boundary internal remaining = case NES.nonEmptySet newBoundary of
      Nothing -> (newInternal, remaining)
      Just nb -> fillUp nb (NES.toSet newInternal) newRemaining
      where
        edgeCandidates = foldMap' cardinalNeighbsSet boundary `S.difference` internal
        newBoundary = edgeCandidates `S.intersection` remaining
        newInternal = NES.withNonEmpty id NES.union internal boundary
        newRemaining = remaining `S.difference` edgeCandidates

-- | The set of unconnected shapes, indexed by their original center of
-- mass
contiguousShapes :: Set Point -> Map (V2 Double) (NESet Point)
contiguousShapes s0 =
  M.fromList
    [ (com, NES.map (subtract topCorner) s)
    | s <- S.toList $ contiguousRegions s0
    , let com = F.fold ((lmap . fmap) fromIntegral F.mean) s
          V2 topCorner _ = boundingBox s
    ]

-- | The set of unconnected shapes, sorted against some function on their
-- original center of masses
contiguousShapesBy ::
  Ord a =>
  (V2 Double -> a) ->
  Set Point ->
  [NESet Point]
contiguousShapesBy f = toList . M.mapKeys f . contiguousShapes

memoPoint :: Memo Point
memoPoint =
  Memo.wrap (uncurry V2) (\(V2 x y) -> (x, y)) $
    Memo.pair Memo.integral Memo.integral

mannDist :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
mannDist x y = sum . abs $ x - y

mannNorm :: (Foldable f, Num a, Num (f a)) => f a -> a
mannNorm = mannDist 0

-- | Treat as complex number multiplication. useful for rotations
mulPoint :: Num a => V2 a -> V2 a -> V2 a
mulPoint (V2 x y) (V2 u v) = V2 (x * u - y * v) (x * v + y * u)

data Dir = North | East | South | West
  deriving stock (Show, Eq, Ord, Generic, Enum)

instance Hashable Dir

instance NFData Dir

instance Finitary Dir

dirPoint :: Num a => Dir -> V2 a
dirPoint = \case
  North -> V2 0 1
  East -> V2 1 0
  South -> V2 0 (-1)
  West -> V2 (-1) 0

-- | 'dirPoint' but with inverted y axis
dirPoint' :: Num a => Dir -> V2 a
dirPoint' = \case
  North -> V2 0 (-1)
  East -> V2 1 0
  South -> V2 0 1
  West -> V2 (-1) 0

-- | Rotate a point by a direction
rotPoint :: Num a => Dir -> V2 a -> V2 a
rotPoint = \case
  North -> id
  East -> \(V2 x y) -> V2 y (-x)
  West -> \(V2 x y) -> V2 (-y) x
  South -> negate

-- | Rotate a point by a direction
rotFin :: KnownNat n => Dir -> FinPoint n -> FinPoint n
rotFin d = over (mapping centeredFinite) (rotPoint d)

centeredFinite :: forall n. KnownNat n => Iso' (Finite n) Rational
centeredFinite =
  iso
    (subtract d . (% 1) . getFinite)
    (Finite . numerator . (+ d))
  where
    d = fromIntegral (natVal (Proxy @n) - 1) % 2

parseDir :: Char -> Maybe Dir
parseDir = flip M.lookup dirMap . toUpper
  where
    dirMap =
      M.fromList
        [ ('N', North)
        , ('E', East)
        , ('S', South)
        , ('W', West)
        , ('U', North)
        , ('R', East)
        , ('D', South)
        , ('L', West)
        , ('V', North)
        , ('>', East)
        , ('^', South)
        , ('<', West)
        ]

parseDir' :: Char -> Dir
parseDir' c = case parseDir c of
  Nothing -> error $ "bad parse dir: " <> [c]
  Just x -> x

-- | Multiply headings, taking North as straight, East as clockwise turn,
-- West as counter-clockwise turn, and South as reverse.
--
-- Should be a commutative group; it's essentially complex number
-- multiplication like 'mulPoint', with North = 1, West = i.  The identity
-- is 'North' and the inverse is the opposite direction.
mulDir :: Dir -> Dir -> Dir
mulDir North = id
mulDir East = \case
  North -> East
  East -> South
  South -> West
  West -> North
mulDir South = \case
  North -> South
  East -> West
  South -> North
  West -> East
mulDir West = \case
  North -> West
  East -> North
  South -> East
  West -> South

allDir :: NonEmpty Dir
allDir = North :| [East ..]

allDirSet :: NESet Dir
allDirSet = NES.fromDistinctAscList allDir

-- | '<>' is 'mulDir'.
instance Semigroup Dir where
  (<>) = mulDir
  stimes n x = case n `mod` 4 of
    1 -> x
    2 -> x <> x
    3 -> invert x
    _ -> North

instance Monoid Dir where
  mempty = North

instance Group Dir where
  invert = \case
    North -> North
    East -> West
    South -> South
    West -> East
  pow = flip stimes

instance Abelian Dir

-- | Represents an orientation of a 2d tile.
data D8 = D8 {d8Rot :: !Dir, d8Flip :: !Bool}
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable D8

instance NFData D8

instance Finitary D8

-- | '<>' is 'mulDir'.
instance Semigroup D8 where
  D8 x1 False <> D8 x2 y2 = D8 (x1 <> x2) y2
  D8 x1 True <> D8 x2 y2 = D8 (x1 <> invert x2) (not y2)

instance Monoid D8 where
  mempty = D8 North False

instance Group D8 where
  invert (D8 x False) = D8 (invert x) False
  invert (D8 x True) = D8 x True

-- | @a `mulD8` b@ represents applying b, then a.
mulD8 :: D8 -> D8 -> D8
mulD8 = (<>)

allD8 :: NonEmpty D8
allD8 = D8 <$> allDir <*> (False :| [True])

allD8Set :: NESet D8
allD8Set = NES.fromDistinctAscList allD8

-- | Rotate and flip a point by a 'D8'
orientPoint :: Num a => D8 -> V2 a -> V2 a
orientPoint = \case
  D8 North False -> id
  D8 East False -> \(V2 x y) -> V2 y (-x)
  D8 West False -> \(V2 x y) -> V2 (-y) x
  D8 South False -> \(V2 x y) -> V2 (-x) (-y)
  D8 North True -> \(V2 x y) -> V2 (-x) y
  D8 East True -> \(V2 x y) -> V2 y x
  D8 West True -> \(V2 x y) -> V2 (-y) (-x)
  D8 South True -> \(V2 x y) -> V2 x (-y)

orientFin :: KnownNat n => D8 -> FinPoint n -> FinPoint n
orientFin d = over (mapping centeredFinite) (orientPoint d)

-- | It's 'Point', but with a newtype wrapper so we have an 'Ord' that
-- sorts by y first, then x
newtype ScanPoint = SP {_getSP :: Point}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num)

instance Hashable ScanPoint

instance NFData ScanPoint

instance Ord ScanPoint where
  compare =
    comparing (view _y . _getSP)
      <> comparing (view _x . _getSP)

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map Point a
parseAsciiMap f = toMapOf (asciiGrid <. folding f)

parseAsciiSet ::
  (Char -> Bool) ->
  String ->
  Set Point
parseAsciiSet f = setOf (asciiGrid . filtered f . asIndex)

asciiGrid :: IndexedTraversal Point String [a] Char a
asciiGrid = conjoined traverse $ \pcfa ->
  sequenceA
    . concat
    . zipWith (\y -> zipWith (\x -> indexed pcfa (V2 x y :: Point)) [0 ..]) [0 ..]
    . lines

displayAsciiMap ::
  -- | default tile
  Char ->
  -- | tile map
  Map Point Char ->
  String
displayAsciiMap d (NEM.IsNonEmpty mp) =
  unlines
    [ [ NEM.findWithDefault d (V2 x y) mp
      | x <- [xMin .. xMax]
      ]
    | y <- [yMin .. yMax]
    ]
  where
    V2 xMin yMin `V2` V2 xMax yMax = boundingBox $ NEM.keysSet mp
displayAsciiMap _ _ = ""

displayAsciiSet ::
  -- | missing tile
  Char ->
  -- | present tile
  Char ->
  -- | tile set
  Set Point ->
  String
displayAsciiSet x y = displayAsciiMap x . M.fromSet (const y)

parseLetters ::
  Set Point ->
  String
parseLetters = fromMaybe (error "parseLetters: no parse") . parseLettersSafe

parseLettersSafe ::
  Set Point ->
  Maybe String
parseLettersSafe = OCR.parseLettersWith (view _x) (view _y)

-- | Lattice points for line between points, not including endpoints
lineTo :: Point -> Point -> [Point]
lineTo p0 p1 = [p0 + t *^ step | t <- [1 .. gcf - 1]]
  where
    d@(V2 dx dy) = p1 - p0
    gcf = gcd dx dy
    step = (`div` gcf) <$> d

instance (Finitary a, KnownNat (Cardinality a * Cardinality a)) => Finitary (V2 a)

instance (Finitary a, KnownNat (Cardinality a * (Cardinality a * Cardinality a))) => Finitary (V3 a)

instance
  (Finitary a, KnownNat ((Cardinality a * Cardinality a) * (Cardinality a * Cardinality a))) =>
  Finitary (V4 a)
