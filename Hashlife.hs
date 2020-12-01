{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.Type.Axioms.Solver #-}

import           Control.Exception
import           Control.Monad
import           Data.Foldable
import qualified Data.HashTable.IO as H
import           Data.Hashable
import           Data.Maybe
import           Data.Proxy
import           Data.Traversable
import           Data.Type.Equality
import           GHC.Exts (Any)
import           GHC.Generics
import           GHC.TypeLits
import           Linear.V3
import           System.Mem.StableName
import           Unsafe.Coerce

import           Util

-- === HASHLIFE === --

data Quad a = Quad { nw :: !a,
                     ne :: !a,
                     sw :: !a,
                     se :: !a }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance Applicative Quad where
  pure a = Quad a a a a
  Quad f g h i <*> Quad a b c d = Quad (f a) (g b) (h c) (i d)

instance Monad Quad where
  return = pure
  Quad a b c d >>= f = Quad (nw $ f a) (ne $ f b) (sw $ f c) (se $ f d)

instance Hashable a => Hashable (Quad a)

-- size = 2^l
data Cell (l :: Nat) a where
  Base :: a -> Cell 0 a
  Cell :: Quad (Cell l a) -> Cell (l + 1) a

type HashTable k v = H.CuckooHashTable k v

memoize :: (Eq name, Hashable name)
        => HashTable name val
        -> (key -> IO name)
        -> (key -> IO val)
        -> key -> IO val
memoize tbl mkname mkval key = do
    sn <- mkname key
    found <- H.lookup tbl sn
    case found of
      Nothing -> do
        val <- mkval key
        H.insert tbl sn val
        return val
      Just v -> return v

type MemoTable l a = HashTable (Quad (StableName (Cell l a))) (Cell (l+1) a)

data MemoTables a = MemoTables {
  consTable :: HashTable (Quad (StableName (Cell Any a))) (Cell Any a),
  calcTable :: HashTable (StableName (Cell Any a)) (Cell Any a),
  baseTable :: HashTable a (Cell 0 a)
  }


  -- memoCons :: forall l. Quad (Cell l a) -> IO (Cell (l + 1) a),
  -- memoCalc :: forall l. KnownNat l
  --          => (Quad (Cell l a) -> IO (Cell l a))
  --          -> (Quad (Cell l a) -> IO (Cell l a))
  -- }

nameQuad :: Quad a -> IO (Quad (StableName a))
nameQuad quad = traverse (makeStableName <=< evaluate) quad

mkTable :: IO (MemoTables a)
mkTable = do
  consTable <- H.new
  calcTable <- H.new
  baseTable <- H.new
  return MemoTables{..}

memoCons :: MemoTables a -> Quad (Cell l a) -> IO (Cell (l + 1) a)
memoCons MemoTables{..} q = look (unsafeCoerce consTable) q
  where
    look :: HashTable (Quad (StableName (Cell l a))) (Cell (l+1) a)
         -> Quad (Cell l a) -> IO (Cell (l+1) a)
    look table q = memoize table nameQuad (pure . Cell) q

nameCell :: Cell l a -> IO (StableName (Cell l a))
nameCell c = makeStableName $! c

memoCalc :: forall l a. (1 <= l, KnownNat l)
         => MemoTables a
         -> (Cell 2 a -> Cell 1 a)
         -> (Cell (l+1) a -> IO (Cell l a))
memoCalc memo@MemoTables{..} f q = look (unsafeCoerce calcTable) q
  where
    look :: HashTable (StableName (Cell (l+1) a)) (Cell l a)
         -> (Cell (l+1) a -> IO (Cell l a))
    look table q = memoize table nameCell (calculate memo f) q

memoBase :: (Hashable a, Eq a)
         => MemoTables a -> a -> IO (Cell 0 a)
memoBase memo@MemoTables{..} a =
  memoize baseTable pure (pure . Base) a

combine = memoCons

level :: forall l a. KnownNat l => Cell l a -> Int
level _ = fromIntegral (natVal (Proxy @ l))

the :: Eq a => Quad a -> a
the (Quad a b c d)
  | a == b && b == c && c == d  =  a
  | otherwise = error "no 'the', fool"

midY :: Quad a -> Quad a -> Quad a
midY (Quad a b c d) (Quad x y z w) = Quad c d x y

midX :: Quad a -> Quad a -> Quad a
midX (Quad a b c d) (Quad x y z w) = Quad b x d z

midXCell :: (1 <= l, KnownNat l) => MemoTables a -> Cell l a -> Cell l a -> IO (Cell l a)
midXCell memo (Cell q1) (Cell q2) = combine memo (midX q1 q2)

midYCell :: (1 <= l, KnownNat l) => MemoTables a -> Cell l a -> Cell l a -> IO (Cell l a)
midYCell memo (Cell q1) (Cell q2) = combine memo (midY q1 q2)

data CmpNatVal a b where
  NatLT :: (a+1 <= b) => CmpNatVal a b
  NatEQ :: (a ~ b) => CmpNatVal a b
  NatGT :: (b+1 <= a) => CmpNatVal a b

cmpNat :: (KnownNat a, KnownNat b)
       => Proxy a -> Proxy b -> CmpNatVal a b
cmpNat pa pb
  | a < b = unsafeCoerce (NatLT :: CmpNatVal 1 2)
  | a == b = unsafeCoerce (NatEQ :: CmpNatVal 1 1)
  | a > b = unsafeCoerce (NatGT :: CmpNatVal 2 1)
  where
    a = natVal pa
    b = natVal pb

-- class (Hashable a, Eq a) => Life a where
--   background :: a
--   step :: V3 (V3 a) -> a

-- mkCell :: forall l a. (KnownNat l, Life a)
--        => MemoTables a -> Map (V2 Int) a
--        -> V2 Int -> IO (Cell l a)
-- mkCell memo grid p = case cmpNat (Proxy @ l) (Proxy @ 0) of
--   NatEQ -> memoBase memo (Map.findWithDefault background p grid)
--   NatGT -> memoBase memo (Map.findWithDefault background p grid)

    -- calc quad@(Quad
    --            (Cell (Quad x11 x21 x12 x22))
    --            (Cell (Quad x31 x41 x32 x42))
    --            (Cell (Quad x13 x23 x14 x24))
    --            (Cell (Quad x33 x44 x33 x44)))
    --   = pure
    --     (Quad
    --      (f (V3
    --          (V3 x11 x21 x31)
    --          (V3 x12 x22 x32)
    --          (V3 x13 x23 x33)))
    --      (f (V3
    --          (V3 x21 x31 x41)
    --          (V3 x22 x32 x42)
    --          (V3 x23 x33 x43)))
    --      (f (V3
    --          (V3 x12 x22 x32)
    --          (V3 x13 x23 x33)
    --          (V3 x14 x24 x34)))
    --      (f (V3
    --          (V3 x22 x32 x42)
    --          (V3 x23 x33 x43)
    --          (V3 x24 x34 x44))))

-- Result is calculated 2^(l-1) steps in the future
-- Size of 2^l (ie. half size of the input).
calculate :: forall l a. (1 <= l, KnownNat l)
          => MemoTables a -> (Cell 2 a -> Cell 1 a)
          -> Cell (l+1) a -> IO (Cell l a)
calculate memo f quad = case cmpNat (Proxy @ l) (Proxy @ 1) of
  NatEQ -> return (f quad)
  NatGT -> case quad of
    Cell (Quad a b c d) -> do
      -- >= 8x8
      north <- midXCell memo a b
      east <- midYCell memo b d
      west <- midYCell memo a c
      south <- midXCell memo c d
      center <- midXCell memo west east

      let
        _ = a :: Cell l a
        _ = north :: Cell l a

      x11 <- memoCalc memo f a
      x21 <- memoCalc memo f north
      x31 <- memoCalc memo f b

      let
        _ = x11 :: Cell (l-1) a

      x12 <- memoCalc memo f west
      x22 <- memoCalc memo f center
      x32 <- memoCalc memo f east

      x13 <- memoCalc memo f c
      x23 <- memoCalc memo f south
      x33 <- memoCalc memo f d

      y1 <- memoCalc memo f =<< combine memo (Quad x11 x21 x12 x22)
      y2 <- memoCalc memo f =<< combine memo (Quad x21 x31 x22 x32)
      y3 <- memoCalc memo f =<< combine memo (Quad x12 x22 x13 x23)
      y4 <- memoCalc memo f =<< combine memo (Quad x22 x32 x23 x33)
      combine memo (Quad y1 y2 y3 y4)

--     calcFlat cell = do mp <- cellToMap (Pos 0 0) cell
--                        let mp' = step mp
--                        mapToCell (Pos 1 1) 1 mp'

-- {-# NOINLINE popTable #-}
-- popTable :: MemoTable (StableName Cell) Int
-- popTable = unsafePerformIO H.new

-- population :: Cell -> IO Int
-- population = memoize popTable makeStableName pop
--   where
--     pop (Zero c) = if elem c [O, T, L] then pure 1 else pure 0
--     pop (Cell _ quad) = do ps <- traverse population quad
--                            return (sum ps)


-- cellToMap :: Pos -> Cell -> IO Grid
-- cellToMap p0 (Zero x) = pure $ if elem x [T,L,O] then
--                                  gridFromList [(p0, x)]
--                                else
--                                  gridFromList []
-- cellToMap p0 cell@(Cell l q) = do
--   pop <- population cell
--   if pop > 0 then do
--     subs <- sequence $ (\cell d -> cellToMap (p0 + d) cell) <$> q <*> dx
--     return (fold subs)
--     else
--     pure mempty
--   where
--     m = 2^(l - 1)
--     dx = Quad (Pos 0 0) (Pos m 0) (Pos 0 m) (Pos m m)

-- blankCell :: Int -> IO Cell
-- blankCell 0 = pure zX
-- blankCell l = do sub <- blankCell (l-1)
--                  combine (pure sub)

-- intersectsOrigin :: Pos -> Int -> Bool
-- intersectsOrigin (Pos x0 y0) l = True --x0 <= 50 && x1 >= 0 && y0 <= 50 && y1 >= 0
--   where
--     x1 = x0 + n
--     y1 = y0 + n
--     n = 2^l

-- mapToCell :: Pos -> Int -> Grid -> IO Cell
-- mapToCell p0 0 grid = pure (canon (indexGrid p0 grid))
-- mapToCell p0 l grid
--   | area (rectIntersection (Rect p0 (p0 + 2^l)) (gridRect grid)) > 0 =
--       combine =<< traverse (\d -> mapToCell (p0+d) (l-1) grid) dx
--   | otherwise             = blankCell l
--   where
--     m = 2^(l - 1)
--     dx = Quad (Pos 0 0) (Pos m 0) (Pos 0 m) (Pos m m)

log2 :: Int -> Int
log2 x
  | 2^y == x  = y
  | otherwise = error "not a power of 2"
  where
    y = round (logBase 2 (fromIntegral x))

-- -- Simulate forward some power-of-two steps in the future.
-- -- Builds overlapping cells of the corresponding size, then stitches together the outputs.
-- simulateFast :: Int -> Grid -> IO Grid
-- simulateFast t grid = do
--   in_pieces <- for in_indexes $ \x -> do
--     mapToCell x l grid

--   out_pieces <- traverse calculate in_pieces
--   out_maps <- traverse (\(x, c) -> cellToMap x c) (Map.toList out_pieces)
--   return (fold out_maps)
--   where
--     -- t = 2^(l-2)
--     l = log2 t + 2
--     xout = 2^(l-1)
--     xin = 2^l

--     xmin = 0
--     xmax = 50
--     ymin = 0
--     ymax = 50

--     -- Blocks of size xin overlapping the input, separated by distance xout so the results do not overlap
--     in_indexes = (\p -> p - Pos (xout`div`2) (xout`div`2)) <$> out_indexes
--     out_indexes = Map.fromList [(Pos x y, Pos x y) |
--                    x <- [xmin, xmin + xout .. xmax],
--                    y <- [ymin, ymin + xout .. ymax]]


-- -- Simulate any arbitrary number of steps into the future by
-- -- recursively calling simulateFast.
-- simulateFast' :: Int -> Grid -> IO Grid
-- simulateFast' 0 state = pure state
-- simulateFast' t state = do next <- simulateFast t0 state
--                            simulateFast' (t - t0) next
--   where
--     t0 = maximum (takeWhile (<=t) [2^l | l <- [0..]])
