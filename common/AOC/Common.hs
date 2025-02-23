{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoDeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- |
-- Module      : AOC.Common
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Common functions for solutions
module AOC.Common (
  trace',
  traceShowIdMsg,
  traceShowMsg,
  preNewline,

  -- * Loops and searches
  iterateMaybe,
  loopMaybe,
  loopMaybeM,
  loopEither,
  firstJust,
  (!!!),
  strictIterate,
  (!?),
  drop',
  dup,
  scanlT,
  scanrT,
  firstRepeated,
  firstRepeatedBy,
  firstRepeatedFinitary,
  firstRepeatedByFinitary,
  findLoopBy,
  findLoop,
  findLoopBy_,
  findLoop_,
  skipConsecutive,
  skipConsecutiveBy,
  fixedPoint,
  floodFill,
  floodFillCount,
  floodFillSteps,
  countTrue,
  pickUnique,
  maybeAlt,

  -- * Lists
  freqs,
  intFreqs,
  lookupFreq,
  lookupIntFreq,
  freqList,
  revFreq,
  bindFreq,
  bindIntFreq,
  perturbations,
  perturbationsBy,
  select,
  slidingWindows,
  withSlidingPairs,
  slidingPairs,
  withLaggedPairs,
  laggedPairs,
  sortedSlidingWindows,
  sortedSlidingWindowsInt,
  middleVal,
  clearOut,
  foldMapPar,
  foldMapPar1,
  foldMapParChunk,
  meanVar,
  maximumVal,
  maximumValBy,
  minimumVal,
  minimumValBy,
  maximumValNE,
  maximumValByNE,
  minimumValNE,
  minimumValByNE,
  listTup,
  _ListTup,
  listTup3,
  _ListTup3,
  listTup4,
  _ListTup4,
  listV2,
  _ListV2,
  listV3,
  _ListV3,
  listV4,
  _ListV4,
  sortSizedBy,
  withAllSized,
  binaryFold,
  binaryFoldPar,
  rotSquare,
  splitHalf,
  splitHalfExact,

  -- * Simple type util
  deleteFinite,
  Letter,
  charFinite,
  _CharFinite,
  hexDigit,
  decimalDigit,
  splitWord,
  digitToIntSafe,
  toBinary,
  toBinaryFixed,
  parseBinary,
  caeser,
  eitherItem,
  chooseEither,
  mapMaybeSet,
  findKeyFor,
  flipMap,
  symDiff,
  unfoldedIterate,
  memo4,
  LCM (..),
  asString,
  numDigits,
  listDigits,
  unListDigits,
  _DigitList,

  -- * Integers
  egcd,
  modInverse,
  bezout,
  inv22Int,
  toNatural,
  factorial,
  integerFactorial,
  pascals,
  triangles,
  triangleNumber,

  -- * Comonad stuff
  matchMap,
  storeMapNeighborhood,
  mapToStore,
  mapFromStore,

  -- * Normal simple line-based
  mapMaybeLines,
  mapMaybeLinesJust,
  traverseLines,
  parseBin,

  -- * Graph
  Graph,
  toFGL,

  -- * Recursion Schemes
  anaM,
)
where

import AOC.Util
import Control.Applicative
import Control.Comonad.Store
import qualified Control.Foldl as F
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Parallel.Strategies
import Data.Bifunctor
import Data.Bit
import Data.Char
import Data.Coerce
import qualified Data.Finitary as F
import Data.Finite
import Data.Finite.Internal
import Data.Foldable
import Data.Function
import Data.Functor.Compose
import qualified Data.Functor.Foldable as R
import qualified Data.Graph.Inductive as G
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntPSQ as IntPSQ
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEM
import Data.Maybe
import qualified Data.MemoCombinators as Memo
import qualified Data.OrdPSQ as OrdPSQ
import Data.Semigroup
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import Data.Traversable
import Data.Tuple
import qualified Data.Type.Nat as N
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Sized as SVG
import qualified Data.Vector.Generic.Sized.Internal as SVG
import qualified Data.Vector.Unboxed.Mutable.Sized as UVM
import Data.Word
import Debug.Trace
import GHC.TypeNats
import Linear (Additive (..), R1 (..), R2 (..), R3 (..), R4 (..), V2 (..), V3 (..), V4 (..), det22, M22)
import qualified Numeric.Lens as L
import Safe

-- | trace but only after something has evaluated to WHNF
trace' :: String -> a -> a
trace' str x = trace (x `seq` str) x

asString :: String -> String
asString = id

-- | Strict (!!)
(!!!) :: [a] -> Int -> a
[] !!! _ = error "Out of range"
(x : _) !!! 0 = x
(x : xs) !!! n = x `seq` (xs !!! (n - 1))

strictIterate :: (a -> a) -> a -> [a]
strictIterate f = go
  where
    go !x = x : go (f x)

-- | Strict drop
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x : xs) = x `seq` drop' (n - 1) xs

-- | Iterate until a 'Nothing' is produced
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = go
  where
    go !x = x : maybe [] go (f x)

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x : _) !? 0 = Just x
(x : xs) !? n = x `seq` (xs !? (n - 1))

-- | Apply function until 'Nothing' is produced, and return last produced
-- value.
loopMaybe ::
  (a -> Maybe a) ->
  a ->
  a
loopMaybe f = go
  where
    go !x = case f x of
      Nothing -> x
      Just !y -> go y

-- | Apply function until a 'Left'.
loopEither ::
  (a -> Either r a) ->
  a ->
  r
loopEither f = go
  where
    go !x = case f x of
      Left r -> r
      Right !y -> go y

-- | Apply monadic function until 'Nothing' is produced, and return last produced
-- value.
loopMaybeM ::
  Monad m =>
  (a -> m (Maybe a)) ->
  a ->
  m a
loopMaybeM f = go
  where
    go !x =
      f x >>= \case
        Nothing -> pure x
        Just !y -> go y

-- | A tuple of the same item twice
dup :: a -> (a, a)
dup x = (x, x)

-- | 'scanl' generalized to all 'Traversable'.
scanlT :: Traversable t => (b -> a -> b) -> b -> t a -> t b
scanlT f z = snd . mapAccumL (\x -> dup . f x) z

-- | 'scanr' generalized to all 'Traversable'.
scanrT :: Traversable t => (a -> b -> b) -> b -> t a -> t b
scanrT f z = snd . mapAccumR (\x -> dup . flip f x) z

-- | Lazily find the first repeated item.
firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated = firstRepeatedBy id

findKeyFor :: Eq a => a -> Map k a -> Maybe k
findKeyFor x = listToMaybe . M.keys . M.filter (== x)

-- | Lazily find the first repeated projection.
firstRepeatedBy :: Ord a => (b -> a) -> [b] -> Maybe b
firstRepeatedBy f = go S.empty
  where
    go seen (x : xs)
      | f x `S.member` seen = Just x
      | otherwise = go (f x `S.insert` seen) xs
    go _ [] = Nothing

-- | firstRepeated but with a bitset
firstRepeatedFinitary :: F.Finitary a => [a] -> Maybe a
firstRepeatedFinitary = firstRepeatedByFinitary id

-- | firstRepeatedBy but with a bitset
firstRepeatedByFinitary :: F.Finitary a => (b -> a) -> [b] -> Maybe b
firstRepeatedByFinitary f xs = runST do
  seen <- UVM.replicate (Bit False)
  res <- runExceptT $ forM_ xs $ \i -> do
    let ixf = F.toFinite (f i)
    Bit found <- seen `UVM.read` ixf
    when found $ throwE i
    UVM.write seen ixf (Bit True)
  pure case res of
    Left x -> Just x
    Right _ -> Nothing

-- | Find a "loop", where applying a function repeatedly creates a closed loop
findLoop :: Ord a => [a] -> Maybe (V2 (Int, a))
findLoop = findLoopBy id

-- | Find a "loop", where applying a function repeatedly creates a closed loop
findLoopBy :: Ord a => (b -> a) -> [b] -> Maybe (V2 (Int, b))
findLoopBy f xs0 =
  torthare xs0 (drop 1 xs0) <&> \(x, xs) ->
    let allLoopVals = S.insert (f x) . S.fromList $ takeWhile (/= f x) (f <$> xs)
        res = do
          (i, restOfLoop) <- uncons $ dropWhile ((`S.notMember` allLoopVals) . f . snd) (zip [0 ..] xs0)
          j <- lastMay $ takeWhile ((`S.member` allLoopVals) . f . snd) restOfLoop
          pure $ V2 i j
     in case res of
          Nothing -> error "findLoopBy: this cannot be"
          Just r -> r
  where
    torthare (x : xs) (y : _ : ys)
      | f x == f y = Just (x, xs)
      | otherwise = torthare xs ys
    torthare _ _ = Nothing

-- | Only indicates if a loop exists.
findLoop_ :: Ord a => [a] -> Bool
findLoop_ = findLoopBy_ id

-- | Only indicates if a loop exists.
findLoopBy_ :: Ord a => (b -> a) -> [b] -> Bool
findLoopBy_ f = isJust . findLoopBy f

skipConsecutive :: Eq a => [a] -> [a]
skipConsecutive = skipConsecutiveBy id

skipConsecutiveBy :: Eq b => (a -> b) -> [a] -> [a]
skipConsecutiveBy _ [] = []
skipConsecutiveBy f (x : xs) = x : go x xs
  where
    go _ [] = []
    go y (z : zs)
      | f y == f z = go y zs
      | otherwise = z : go z zs

-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
      | x == y = x
      | otherwise = go y
      where
        y = f x

-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

-- | Given a map of @k@ to possible @a@s for that @k@, find possible
-- configurations where each @k@ is given its own unique @a@.
pickUnique :: (Ord k, Ord a) => [(k, Set a)] -> [Map k a]
pickUnique mp = flip evalStateT S.empty $ do
  fmap M.fromList . for opts . traverse $ \poss -> do
    seen <- get
    pick <- lift $ S.toList (poss `S.difference` seen)
    pick <$ modify (S.insert pick)
  where
    opts = L.sortOn (S.size . snd) mp

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

-- | Build a frequency map
intFreqs :: Foldable f => f Int -> IntMap Int
intFreqs = IM.fromListWith (+) . map (,1) . toList

-- | each item paired with the list not including that item
select :: [a] -> [(a, [a])]
select = go []
  where
    go _ [] = []
    go xs (y : ys) = (y, xs ++ ys) : go (y : xs) ys

-- | Look up a count from a frequency map, defaulting to zero if item is
-- not foudn
lookupFreq :: Ord a => a -> Map a Int -> Int
lookupFreq = M.findWithDefault 0

-- | Look up a count from a frequency map, defaulting to zero if item is
-- not foudn
lookupIntFreq :: Int -> IntMap Int -> Int
lookupIntFreq = IM.findWithDefault 0

-- | Build a reverse frequency map
revFreq :: (Foldable f, Ord a) => f a -> IntMap (NESet a)
revFreq =
  IM.fromListWith (<>)
    . map (swap . first NES.singleton)
    . M.toList
    . freqs

flipMap :: (Ord k, Ord a) => Map k a -> Map a (Set k)
flipMap = M.fromListWith (<>) . map (\(k, v) -> (v, S.singleton k)) . M.toList

bindFreq :: Ord b => Map a Int -> (a -> Map b Int) -> Map b Int
bindFreq mp f = M.unionsWith (+) [(* n) <$> f x | (x, n) <- M.toList mp]

bindIntFreq :: IntMap Int -> (Int -> IntMap Int) -> IntMap Int
bindIntFreq mp f = IM.unionsWith (+) [(* n) <$> f x | (x, n) <- IM.toList mp]

-- | Build a list of /descending/ frequencies.  Ties are sorted.
freqList :: (Foldable f, Ord a) => f a -> [(Int, a)]
freqList = concatMap (traverse toList) . IM.toDescList . revFreq

eitherItem :: Lens' (Either a a) a
eitherItem f (Left x) = Left <$> f x
eitherItem f (Right x) = Right <$> f x

chooseEither :: Alternative f => f a -> f b -> f (Either a b)
chooseEither x y = (Left <$> x) <|> (Right <$> y)

splitWord :: Word8 -> (Finite 16, Finite 16)
splitWord = swap . separateProduct . F.toFinite

decimalDigit :: Prism' Char (Finite 10)
decimalDigit = prism' _to _from
  where
    _to = intToDigit . fromIntegral
    _from c
      | isDigit c = Just (Finite (fromIntegral (digitToInt c)))
      | otherwise = Nothing

hexDigit :: Prism' Char (Finite 16)
hexDigit = prism' _to _from
  where
    _to = intToDigit . fromIntegral
    _from c
      | isHexDigit c = Just (Finite (fromIntegral (digitToInt c)))
      | otherwise = Nothing

type Letter = Finite 26

-- | Parse a letter into a number 0 to 25.  Returns 'False' if lowercase
-- and 'True' if uppercase.
charFinite :: Char -> Maybe (Bool, Finite 26)
charFinite (ord -> c) =
  asum
    [ (False,) <$> packFinite (fromIntegral (c - ord 'a'))
    , (True,) <$> packFinite (fromIntegral (c - ord 'A'))
    ]

digitToIntSafe :: Char -> Maybe Int
digitToIntSafe c = digitToInt c <$ guard (isDigit c || toLower c `elem` ("abcdef" :: String))

parseBinary :: [Bool] -> Int
parseBinary = foldl' (\i b -> if b then i * 2 + 1 else i * 2) 0

toBinary :: Int -> [Bool]
toBinary x = go x []
  where
    go i = rest . ((b /= 0) :)
      where
        (a, b) = i `divMod` 2
        rest
          | a == 0 = id
          | otherwise = go a

-- fixed number of digits, pad with 0
toBinaryFixed :: Int -> Int -> [Bool]
toBinaryFixed d x
  | d <= 0 = []
  | otherwise = (a /= 0) : toBinaryFixed (d - 1) b
  where
    (a, b) = x `divMod` (2 ^ (d - 1))

-- | Prism for a 'Char' as @('Bool', 'Finite' 26)@, where the 'Finite' is
-- the letter parsed as a number from 0 to 25, and the 'Bool' is lowercase
-- ('False') or uppercase ('True').
_CharFinite :: Prism' Char (Bool, Finite 26)
_CharFinite = prism' fromcf charFinite
  where
    fromcf (c, x) = chr $ fromIntegral x + ord b
      where
        b
          | c = 'A'
          | otherwise = 'a'

-- | Caeser shift, preserving case.  If you have an 'Int' or 'Integer',
-- convert into 'Finite' using 'modulo'.
caeser :: Finite 26 -> Char -> Char
caeser i = over (_CharFinite . _2) (+ i)

-- | Collect all possible single-item perturbations from a given
-- perturbing function.
--
-- > perturbations (\i -> [i - 1, i + 1]) [0,10,100]
--      == [ [-1,10,100]
--         , [ 1,10,100]
--         , [ 0, 9,100]
--         , [ 0,11,100]
--         , [ 0,10, 99]
--         , [ 0,10,101]
--         ]
perturbations ::
  Traversable f =>
  (a -> [a]) ->
  f a ->
  [f a]
perturbations = perturbationsBy traverse

-- | Collect all possible single-item perturbations from a given
-- perturbing function.
--
-- > perturbations (\i -> [i - 1, i + 1]) [0,10,100]
--      == [ [-1,10,100]
--         , [ 1,10,100]
--         , [ 0, 9,100]
--         , [ 0,11,100]
--         , [ 0,10, 99]
--         , [ 0,10,101]
--         ]
perturbationsBy ::
  Conjoined p =>
  Over p (Bazaar p a a) s t a a ->
  (a -> [a]) ->
  s ->
  [t]
perturbationsBy p f = experiment f <=< holesOf p

-- | Clear out characters not matching a predicate
clearOut :: (Char -> Bool) -> String -> String
clearOut p = map $ \c ->
  if p c
    then ' '
    else c

withLaggedPairs :: Int -> (a -> a -> b) -> [a] -> [b]
withLaggedPairs n f xs = zipWith f xs (drop n xs)

laggedPairs :: Int -> [a] -> [(a, a)]
laggedPairs n = withLaggedPairs n (,)

withSlidingPairs :: (a -> a -> b) -> [a] -> [b]
withSlidingPairs = withLaggedPairs 1

slidingPairs :: [a] -> [(a, a)]
slidingPairs = laggedPairs 1

-- | sliding windows of a given length
slidingWindows :: Int -> [a] -> [Seq a]
slidingWindows n = uncurry go . first Seq.fromList . splitAt n
  where
    go ws@(_ :<| qs) = \case
      x : xs -> ws : go (qs :|> x) xs
      [] -> [ws]
    go _ = const []

-- | sorted windows of a given length
sortedSlidingWindows ::
  forall k v.
  Ord k =>
  Int ->
  [(k, v)] ->
  [OrdPSQ.OrdPSQ k Int v]
sortedSlidingWindows n = uncurry go . first OrdPSQ.fromList . splitAt n . zipWith reIx [0 ..]
  where
    reIx i (j, k) = (j, i, k)
    go :: OrdPSQ.OrdPSQ k Int v -> [(k, Int, v)] -> [OrdPSQ.OrdPSQ k Int v]
    go ws = \case
      (k, i, x) : xs -> ws : go (OrdPSQ.insert k i x (OrdPSQ.deleteMin ws)) xs
      _ -> [ws]

-- | sorted windows of a given length
sortedSlidingWindowsInt ::
  forall v.
  () =>
  Int ->
  [(Int, v)] ->
  [IntPSQ.IntPSQ Int v]
sortedSlidingWindowsInt n = uncurry go . first IntPSQ.fromList . splitAt n . zipWith reIx [0 ..]
  where
    reIx i (j, k) = (j, i, k)
    go :: IntPSQ.IntPSQ Int v -> [(Int, Int, v)] -> [IntPSQ.IntPSQ Int v]
    go ws = \case
      (k, i, x) : xs -> ws : go (IntPSQ.insert k i x (IntPSQ.deleteMin ws)) xs
      _ -> [ws]

-- | Gets at the middle index, rounded up
middleVal :: [a] -> Maybe a
middleVal xs0 = go xs0 xs0
  where
    go (_ : xs) (_ : _ : ys) = go xs ys
    go (x : _) _ = Just x
    go [] _ = Nothing

-- | Get the key-value pair corresponding to the maximum value in the map
maximumVal :: Ord b => Map a b -> Maybe (a, b)
maximumVal = maximumValBy compare

-- | Get the key-value pair corresponding to the maximum value in the map,
-- with a custom comparing function.
--
-- > 'maximumVal' == 'maximumValBy' 'compare'
maximumValBy :: (b -> b -> Ordering) -> Map a b -> Maybe (a, b)
maximumValBy c =
  fmap (maximumBy (c `on` snd))
    . NE.nonEmpty
    . M.toList

-- | Get the key-value pair corresponding to the minimum value in the map,
-- with a custom comparing function.
--
-- > 'minimumVal' == 'minimumValBy' 'compare'
minimumValBy :: (b -> b -> Ordering) -> Map a b -> Maybe (a, b)
minimumValBy c =
  fmap (minimumBy (c `on` snd))
    . NE.nonEmpty
    . M.toList

-- | Get the key-value pair corresponding to the minimum value in the map
minimumVal :: Ord b => Map a b -> Maybe (a, b)
minimumVal = minimumValBy compare

-- | Version of 'maximumValBy' for nonempty maps.
maximumValByNE :: (b -> b -> Ordering) -> NEMap a b -> (a, b)
maximumValByNE c =
  maximumBy (c `on` snd)
    . NEM.toList

-- | Version of 'maximumVal' for nonempty maps.
maximumValNE :: Ord b => NEMap a b -> (a, b)
maximumValNE = maximumValByNE compare

-- | Version of 'minimumValBy' for nonempty maps.
minimumValByNE :: (b -> b -> Ordering) -> NEMap a b -> (a, b)
minimumValByNE c =
  minimumBy (c `on` snd)
    . NEM.toList

-- | Version of 'minimumVal' for nonempty maps.
minimumValNE :: Ord b => NEMap a b -> (a, b)
minimumValNE = minimumValByNE compare

foldMapParChunk ::
  forall a m.
  (NFData m, Monoid m) =>
  -- | chunk size
  Int ->
  (a -> m) ->
  [a] ->
  m
foldMapParChunk n f xs =
  fold $
    parMap rdeepseq (foldMap f) (chunksOf n xs)

binaryFold ::
  Monoid m =>
  -- | minimum size list
  Int ->
  (a -> m) ->
  [a] ->
  m
binaryFold n f = bigGo (1 :: Int)
  where
    bigGo i xs = case go i xs of
      (!r, []) -> r
      (!r, ys) -> r <> bigGo (i + 1) ys
    go 1 xs = first (foldMap f) (splitAt n xs)
    go i xs = (t, zs)
      where
        !t = r <> s
        (r, ys) = go (i - 1) xs
        (s, zs) = go (i - 1) ys

binaryFoldPar ::
  Monoid m =>
  -- | minimum size list
  Int ->
  (a -> m) ->
  [a] ->
  m
binaryFoldPar n f = runEval . bigGo (1 :: Int)
  where
    bigGo i xs = do
      (!r, ys) <- go i xs
      case ys of
        [] -> pure r
        _ : _ -> do
          q <- bigGo (i + 1) ys
          pure (q <> r)
    go 1 xs = (,zs) <$> rpar (foldMap f ys)
      where
        (ys, zs) = splitAt n xs
    go i xs = do
      (r, ys) <- go (i - 1) xs
      (s, zs) <- go (i - 1) ys
      let !t = r <> s
      pure (t, zs)

-- | Flip list 90 degrees CCW
rotSquare :: [[a]] -> [[a]]
rotSquare = reverse . L.transpose

listTup :: [a] -> Maybe (a, a)
listTup (x : y : _) = Just (x, y)
listTup _ = Nothing

_ListTup :: Prism' [a] (a, a)
_ListTup = prism' (\(x, y) -> [x, y]) $ \case
  [x, y] -> Just (x, y)
  _ -> Nothing

listV2 :: [a] -> Maybe (V2 a)
listV2 (x : y : _) = Just $ V2 x y
listV2 _ = Nothing

_ListV2 :: Prism' [a] (V2 a)
_ListV2 = prism' (\(V2 x y) -> [x, y]) $ \case
  [x, y] -> Just $ V2 x y
  _ -> Nothing

listTup3 :: [a] -> Maybe (a, a, a)
listTup3 (x : y : z : _) = Just (x, y, z)
listTup3 _ = Nothing

_ListTup3 :: Prism' [a] (a, a, a)
_ListTup3 = prism' (\(x, y, z) -> [x, y, z]) $ \case
  [x, y, z] -> Just (x, y, z)
  _ -> Nothing

listV3 :: [a] -> Maybe (V3 a)
listV3 (x : y : z : _) = Just $ V3 x y z
listV3 _ = Nothing

_ListV3 :: Prism' [a] (V3 a)
_ListV3 = prism' (\(V3 x y z) -> [x, y, z]) $ \case
  [x, y, z] -> Just $ V3 x y z
  _ -> Nothing

listTup4 :: [a] -> Maybe (a, a, a, a)
listTup4 (x : y : z : k : _) = Just (x, y, z, k)
listTup4 _ = Nothing

_ListTup4 :: Prism' [a] (a, a, a, a)
_ListTup4 = prism' (\(x, y, z, k) -> [x, y, z, k]) $ \case
  [x, y, z, k] -> Just (x, y, z, k)
  _ -> Nothing

listV4 :: [a] -> Maybe (V4 a)
listV4 (x : y : z : k : _) = Just $ V4 x y z k
listV4 _ = Nothing

_ListV4 :: Prism' [a] (V4 a)
_ListV4 = prism' (\(V4 x y z k) -> [x, y, z, k]) $ \case
  [x, y, z, k] -> Just $ V4 x y z k
  _ -> Nothing

-- | Delete a potential value from a 'Finite'.
deleteFinite ::
  KnownNat n =>
  Finite (n + 1) ->
  Finite (n + 1) ->
  Maybe (Finite n)
deleteFinite n m = case n `cmp` m of
  LT -> unshift m
  EQ -> Nothing
  GT -> strengthen m

-- | 'foldMap', but in parallel.
foldMapPar :: Monoid b => (a -> b) -> [a] -> b
foldMapPar f = runEval . fmap mconcat . traverse (rpar . f)

-- | 'foldMap1', but in parallel.
foldMapPar1 :: Semigroup b => (a -> b) -> NonEmpty a -> b
foldMapPar1 f = runEval . fmap sconcat . traverse (rpar . f)

-- | 'F.Fold' for computing mean and variance
meanVar :: Fractional a => F.Fold a (a, a)
meanVar = do
  n <- fromIntegral <$> F.length
  x <- F.sum
  x2 <- lmap (^ (2 :: Int)) F.sum
  pure $
    let μ = x / n
        σ2 = x2 / n - μ * μ
     in (μ, σ2)

-- | Flood fill from a starting set
floodFill ::
  Ord a =>
  -- | Expansion (be sure to limit allowed points)
  (a -> Set a) ->
  -- | Start points
  Set a ->
  -- | Flood filled
  Set a
floodFill f = snd . floodFillCount f

-- | Flood fill from a starting set, counting the number of steps
floodFillCount ::
  Ord a =>
  -- | Expansion (be sure to limit allowed points)
  (a -> Set a) ->
  -- | Start points
  Set a ->
  -- | Flood filled, with count of number of steps
  (Int, Set a)
floodFillCount f = go 0 S.empty
  where
    go !n !innr !outr
      | S.null outr' = (n, innr')
      | otherwise = go (n + 1) innr' outr'
      where
        innr' = S.union innr outr
        outr' = foldMap f outr `S.difference` innr'

-- | Flood fill from a starting set, with the shortest distance
floodFillSteps ::
  Ord a =>
  -- | Expansion (be sure to limit allowed points)
  (a -> Set a) ->
  -- | Start points
  Set a ->
  -- | Flood filled, with count of number of steps
  Map a Int
floodFillSteps f = go 0 M.empty
  where
    go !n !innr !outr
      | S.null outr' = innr'
      | otherwise = go (n + 1) innr' outr'
      where
        innr' = innr <> M.fromSet (const n) outr
        outr' = foldMap f outr `S.difference` M.keysSet innr'

type Graph v e = Map v (Map v e)

toFGL :: (G.Graph gr, Ord v) => Graph v e -> (gr v e, Set v)
toFGL gr =
  ( G.mkGraph
      (zip [0 ..] $ toList vertices)
      ((\(v, u, e) -> (ixOf v, ixOf u, e)) <$> edges)
  , vertices
  )
  where
    edges = do
      (v, es) <- M.toList gr
      (u, e) <- M.toList es
      pure (v, u, e)
    vertices = foldMap (\(v, u, _) -> S.fromList [v, u]) edges
    ixOf = (`S.findIndex` vertices)

-- data ExpGraph v e = ExpGraph (Map v )
-- data ExpGraph v e = ExpGraph e (Map v (ExpGraph v e))

-- data ExpGraph v e = ExpGraph (Map v (e, ExpGraph v e))
-- type ExpGraph v e = Map v (Map v (e, ExpGraph
-- data ExpGraph v e = ExpGraph v (Map v (e, ExpGraph v e))
-- { expGraphMap :: Map v [(v, e, ExpGraph v e)] }
-- -- newtype ExpGraph v e = ExpGraph { expGraphMap :: Map v [(v, e, ExpGraph v e)] }
--   deriving (Show, Eq, Ord, Functor)
-- R.makeBaseFunctor ''ExpGraph

-- -- Map v [(e, v)]

-- expandGraph :: forall v e. Ord v => Graph v e -> Map v (ExpGraph v e)
-- expandGraph gr = M.mapWithKey go gr
-- where
--   go :: v -> Map v e -> ExpGraph v e
--   go
--   -- where
--   go :: Map v e -> ExpandGrahF v e (Map v e)
--   go vs = ExpandGraph

-- expandGraph :: forall v e. Ord v => Graph v e -> ExpGraph v e
-- expandGraph gr = go (M.keysSet gr)
--   where
--     go vs = ExpGraph $ M.fromSet (_ . flip M.lookup gr) vs

-- expandGraph gr = R.ana go (M.keysSet gr)
--   where
--     go :: Set v -> ExpGraphF v e (Set v)
--     go vs = ExpGraphF $
--       M.mapMaybe id $ M.fromSet (fmap () . flip M.lookup gr) vs
--       -- M.fromSet (_ . map swap . foldMap M.toList . flip M.lookup gr) vs
--     -- M.fromSet (_ . map swap . foldMap M.toList . flip M.lookup gr) vs

-- -- | Recursively fold up a monoid value for each vertex and all of its
-- -- children's monoid values.  You can transform the value in-transit before
-- -- it is accumulated if you want.
-- foldMapGraph
--     :: (Ord v, Monoid m)
--     => (v -> m)         -- ^ embed the vertex
--     -> (e -> m -> m)    -- ^ transform with edge before it is accumulated
--     -> Graph v e
--     -> Map v m
-- foldMapGraph f g gr = res
--   where
--     res = M.foldMapWithKey (\s v -> f s <> foldMap (g v) (M.lookup s res))
--        <$> gr

-- data ExpandGraph v e = ExpandGraph v e (ExpandGraph v e)

-- expandGraph :: Ord v => Graph v e -> Map v (v, [ExpandGraph v e])
-- expandGraph gr = M.mapWithKey
--   (\v es ->
--       ( v
--       , (\(u,e) -> ExpandGraph u e (go (gr M.! u)))
--         <$> M.toList es
--       )
--   )
--   gr

sortSizedBy ::
  VG.Vector v a =>
  (a -> a -> Ordering) ->
  SVG.Vector v n a ->
  SVG.Vector v n a
sortSizedBy f (SVG.Vector xs) = runST $ do
  ys <- VG.thaw xs
  VAI.sortBy f ys
  SVG.Vector <$> VG.unsafeFreeze ys
{-# INLINE sortSizedBy #-}

withAllSized ::
  VG.Vector v a =>
  NonEmpty [a] ->
  (forall n. KnownNat n => NonEmpty (SVG.Vector v n a) -> Maybe r) ->
  Maybe r
withAllSized (x :| xs) f = SVG.withSizedList x $ \vx ->
  f . (vx :|) =<< traverse SVG.fromList xs
{-# INLINE withAllSized #-}

type instance Index (SVG.Vector v n a) = Int

type instance IxValue (SVG.Vector v n a) = a

instance (Ixed (v a), Index (v a) ~ Int, IxValue (v a) ~ a) => Ixed (SVG.Vector v n a) where
  ix i f (SVG.Vector v) = SVG.Vector <$> ix i f v

instance (KnownNat n, forall a. VG.Vector v a, 1 <= n) => R1 (SVG.Vector v n) where
  _x = SVG.ix 0

instance (KnownNat n, forall a. VG.Vector v a, 2 <= n) => R2 (SVG.Vector v n) where
  _xy f v = (\(V2 x y) -> v SVG.// [(0, x), (1, y)]) <$> f (V2 (v `SVG.index` 0) (v `SVG.index` 1))
  _y = SVG.ix 1

instance (KnownNat n, forall a. VG.Vector v a, 3 <= n) => R3 (SVG.Vector v n) where
  _xyz f v =
    (\(V3 x y z) -> v SVG.// [(0, x), (1, y), (2, z)])
      <$> f (V3 (v `SVG.index` 0) (v `SVG.index` 1) (v `SVG.index` 2))
  _z = SVG.ix 2

instance (KnownNat n, forall a. VG.Vector v a, 4 <= n) => R4 (SVG.Vector v n) where
  _xyzw f v =
    (\(V4 x y z w) -> v SVG.// [(0, x), (1, y), (2, z), (3, w)])
      <$> f (V4 (v `SVG.index` 0) (v `SVG.index` 1) (v `SVG.index` 2) (v `SVG.index` 3))
  _w = SVG.ix 3

type instance Index (OrdPSQ.OrdPSQ k p v) = k

type instance IxValue (OrdPSQ.OrdPSQ k p v) = v

instance (Ord k, Ord p) => Ixed (OrdPSQ.OrdPSQ k p v) where
  ix i f q = case OrdPSQ.lookup i q of
    Nothing -> pure q
    Just (p, x) -> flip (OrdPSQ.insert i p) q <$> f x

-- | parse a binary string
--
-- this is just here for me to remember that i can use lens combinators lol
parseBin :: String -> Maybe Int
parseBin = preview (L.base 2)

matchMap :: (Num k, Eq a) => Map k a -> Store k (Maybe a) -> Bool
matchMap mp = getAll . storeMapNeighborhood (fmap All . (==) . Just <$> mp)

-- | 'extend' this to get a convolution
storeMapNeighborhood :: (Num k, Monoid b) => Map k (Maybe a -> b) -> Store k (Maybe a) -> b
storeMapNeighborhood mp x = M.foldMapWithKey (\p f -> f $ peeks (+ p) x) mp

mapToStore :: (Ord k, Num k) => Map k a -> Store k (Maybe a)
mapToStore mp = store (`M.lookup` mp) 0

mapFromStore :: Num k => Set k -> Store k a -> Map k a
mapFromStore ks = experiment \x -> M.fromSet (+ x) ks

mapMaybeLines :: (String -> Maybe a) -> String -> [a]
mapMaybeLines f = mapMaybe f . lines

mapMaybeLinesJust :: (String -> Maybe a) -> String -> Maybe [a]
mapMaybeLinesJust f = Just . mapMaybeLines f

traverseLines :: (String -> Maybe a) -> String -> Maybe [a]
traverseLines f = traverse f . lines

toNatural :: Integral a => a -> Maybe Natural
toNatural x = fromIntegral x <$ guard (x >= 0)

factorial :: Int -> Int
factorial n = go 2 1
  where
    go i !x
      | i > n = x
      | otherwise = go (i + 1) (x * i)

-- | case egcd a b of
--      (d, u, v) ->
--          u * a + v * b = d
--       && d == gcd(a,b)
--
-- from arithmoi library
egcd :: Integral a => a -> a -> (a, a, a)
egcd a b = (d, u, v)
  where
    (d, x, y) = eGCD 0 1 1 0 (abs a) (abs b)
    u
      | a < 0 = negate x
      | otherwise = x
    v
      | b < 0 = negate y
      | otherwise = y
    eGCD !n1 o1 !n2 o2 r s
      | s == 0 = (r, o1, o2)
      | otherwise = case r `quotRem` s of
          (q, t) -> eGCD (o1 - q * n1) n1 (o2 - q * n2) n2 s t
{-# SPECIALIZE egcd ::
  Int -> Int -> (Int, Int, Int)
  , Word -> Word -> (Word, Word, Word)
  , Integer -> Integer -> (Integer, Integer, Integer)
  #-}

-- | modInverse(a,b) is (a^-1 in Z_b, b^-1 in Z_a)
modInverse :: Integral a => a -> a -> Maybe (a, a)
modInverse x y = case egcd x y of
  (1, u, v) -> Just (u, v)
  _ -> Nothing

-- | gives (V2 (V2 mx bx) (V2 my by)), where x solutions are (mx k + bx) and y
-- solutions are (my k + by)
bezout :: Integral a => a -> a -> a -> Maybe (V2 (V2 a))
bezout a b c
  | r == 0 =
      Just $
        V2
          (V2 (b `div` d) (u * c'))
          (V2 (- (a `div` d)) (v * c'))
  | otherwise = Nothing
  where
    (d, u, v) = egcd a b
    (c', r) = c `divMod` d
{-# SPECIALIZE bezout ::
  Int -> Int -> Int -> Maybe (V2 (V2 Int)),
  Word -> Word -> Word -> Maybe (V2 (V2 Word)),
  Integer -> Integer -> Integer -> Maybe (V2 (V2 Integer))
  #-}

-- | Returns det(A) and inv(A)det(A)
inv22Int :: (Num a, Eq a) => M22 a -> Maybe (a, M22 a)
inv22Int m@(V2 (V2 a b) (V2 c d))
  | det == 0 = Nothing
  | otherwise = Just (det, V2 (V2 d (-b)) (V2 (-c) a))
  where
    det = det22 m
{-# SPECIALIZE inv22Int ::
   M22 Int -> Maybe (Int, M22 Int),
   M22 Word -> Maybe (Word, M22 Word),
   M22 Integer -> Maybe (Integer, M22 Integer)
  #-}

integerFactorial :: Integer -> Integer
integerFactorial n = go 2 1
  where
    go i !x
      | i > n = x
      | otherwise = go (i + 1) (x * i)

pascals :: [[Int]]
pascals = repeat 1 : map (drop 1 . L.scanl' (+) 0) pascals

-- | the triangular numbers
--
-- triangles !! n = 1+2+..+n = (n * (n+1))/2
triangles :: [Int]
triangles = 0 : (pascals !! 2)

triangleNumber :: Int -> Int
triangleNumber n = (n * (n + 1)) `div` 2

mapMaybeSet :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapMaybeSet f = S.fromList . mapMaybe f . S.toList

symDiff :: Ord a => Set a -> Set a -> Set a
symDiff x y = (x `S.union` y) S.\\ (x `S.intersection` y)

numDigits :: (Ord a, Num a) => a -> Int
numDigits x = length . takeWhile (<= x) $ iterate (* 10) 1

listDigits :: (Integral a) => a -> [a]
listDigits = L.unfoldr \x ->
  let (a, b) = x `divMod` 10
   in [(b, a) | b > 0]

unListDigits :: Num a => [a] -> a
unListDigits = foldr (\x acc -> x + acc * 10) 0

_DigitList :: Integral a => Iso' a [a]
_DigitList = iso listDigits unListDigits

memo4 ::
  Memo.Memo a ->
  Memo.Memo b ->
  Memo.Memo c ->
  Memo.Memo d ->
  (a -> b -> c -> d -> r) ->
  (a -> b -> c -> d -> r)
memo4 a b c d = a . (Memo.memo3 b c d .)

anaM ::
  (Monad m, R.Corecursive t, Traversable (R.Base t)) =>
  (a -> m (R.Base t a)) ->
  a ->
  m t
anaM f = R.hylo (fmap R.embed . sequenceA <=< getCompose) (Compose . f)

newtype Iterate n a = Iterate {runIterate :: a}

unfoldedIterate ::
  forall n a proxy.
  N.SNatI n =>
  proxy n ->
  (a -> a) ->
  a ->
  a
unfoldedIterate _ f x = runIterate (N.induction1 start step :: Iterate n a)
  where
    start :: Iterate 'N.Z a
    start = Iterate x
    step :: Iterate m a -> Iterate ('N.S m) a
    step = coerce f

instance FunctorWithIndex Int v => FunctorWithIndex (Finite n) (SVG.Vector v n) where
  imap f (SVG.Vector xs) = SVG.Vector $ imap (f . Finite . fromIntegral) xs

instance FoldableWithIndex Int v => FoldableWithIndex (Finite n) (SVG.Vector v n) where
  ifoldMap f (SVG.Vector xs) = ifoldMap (f . Finite . fromIntegral) xs

instance TraversableWithIndex Int v => TraversableWithIndex (Finite n) (SVG.Vector v n) where
  itraverse f (SVG.Vector xs) = SVG.Vector <$> itraverse (f . Finite . fromIntegral) xs

instance (Functor v, KnownNat n, forall a. VG.Vector v a) => Additive (SVG.Vector v n) where
  zero = SVG.replicate 0
  x ^+^ y = SVG.zipWith (+) x y
  x ^-^ y = SVG.zipWith (-) x y
  liftU2 = SVG.zipWith
  liftI2 = SVG.zipWith

-- instance Hashable a => Hashable (Seq a) where
--     hashWithSalt s = hashWithSalt s . toList
--     hash = hash . toList

instance FunctorWithIndex k (NEMap k) where
  imap = NEM.mapWithKey

instance FoldableWithIndex k (NEMap k) where
  ifoldMap = NEM.foldMapWithKey

instance TraversableWithIndex k (NEMap k) where
  itraverse = NEM.traverseWithKey

-- | Generalize a 'Maybe' to any 'Alternative'
maybeAlt :: Alternative m => Maybe a -> m a
maybeAlt = maybe empty pure

splitHalf :: [a] -> ([a], [a])
splitHalf xs = go xs xs
  where
    go (y : ys) (_ : _ : zs) = (y :) `first` go ys zs
    go ys _ = ([], ys)

splitHalfExact :: [a] -> Maybe ([a], [a])
splitHalfExact xs = go xs xs
  where
    go (y : ys) (_ : _ : zs) = first (y :) <$> go ys zs
    go ys [] = Just ([], ys)
    go _ (_ : _) = Nothing

-- | Like 'traceShowId' but with an extra message
traceShowIdMsg :: Show a => String -> a -> a
traceShowIdMsg msg x = trace (msg ++ show x) x

-- | Like 'traceShow' but with an extra message
traceShowMsg :: Show a => String -> a -> b -> b
traceShowMsg msg x = trace (msg ++ show x)

-- | Useful for aligning debugger output
preNewline :: String -> String
preNewline = ('\n' :)

newtype LCM a = LCM {getLCM :: a}

instance Integral a => Monoid (LCM a) where
  mempty = LCM 1

instance Integral a => Semigroup (LCM a) where
  LCM x <> LCM y = LCM (lcm x y)
