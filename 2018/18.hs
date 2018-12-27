#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.HashTable.IO as H
import           Data.Hashable
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ord
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Generics
import           System.IO.Unsafe
import           System.Mem.StableName

import           Util.Util


data Pos = Pos {
  x :: Int,
  y :: Int
  }
  deriving (Show, Eq, Ord)

instance Num Pos where
  fromInteger n = Pos (fromInteger n) (fromInteger n)
  Pos x1 y1 + Pos x2 y2 = Pos (x1 + x2) (y1 + y2)
  Pos x1 y1 - Pos x2 y2 = Pos (x1 - x2) (y1 - y2)
  Pos x1 y1 * Pos x2 y2 = Pos (x1 * x2) (y1 * y2)
  abs _ = undefined
  signum _ = undefined

neighbors :: Pos -> [Pos]
neighbors p = [p + Pos dx dy
              | dx <- [-1, 0, 1],
                dy <- [-1, 0, 1],
                abs dx + abs dy > 0]

-- Open, Trees, Lumberyard
data Square = O | T | L | X
  deriving (Show, Eq, Ord)

type Grid = Map Pos Square

count :: Eq a => a -> [a] -> Int
count a xs = length [x | x <- xs, x == a]

stepSquare :: Square -> [Square] -> Square
stepSquare s neighbors = case s of
  O
    | count T neighbors >= 3 -> T
    | otherwise              -> O
  T
    | count L neighbors >= 3 -> L
    | otherwise              -> T
  L
    | elem L neighbors && elem T neighbors -> L
    | otherwise              -> O
  X -> X

step :: Grid -> Grid
step grid = Map.fromList $ do
  (p, s) <- Map.toList grid
  let xs = [Map.findWithDefault X n grid | n <- neighbors p]
  return (p, stepSquare s xs)

-- === HASHLIFE === --

data Quad a = Quad !a !a !a !a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance Applicative Quad where
  pure a = Quad a a a a
  Quad f g h i <*> Quad a b c d = Quad (f a) (g b) (h c) (i d)

instance Hashable a => Hashable (Quad a)

data Cell = Zero Square | Cell !Int (Quad Cell)
  deriving (Show, Eq)

type MemoTable k v = H.CuckooHashTable k v

zO, zT, zL, zX :: Cell
{-# NOINLINE zO #-}
zO = Zero O
{-# NOINLINE zT #-}
zT = Zero T
{-# NOINLINE zL #-}
zL = Zero L
{-# NOINLINE zX #-}
zX = Zero X

canon :: Square -> Cell
canon O = zO
canon T = zT
canon L = zL
canon X = zX

memoize :: (Eq name, Hashable name)
        => MemoTable name val
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

level :: Cell -> Int
level (Zero _) = 0
level (Cell n _) = n

{-# NOINLINE consTable #-}
consTable :: MemoTable (Quad (StableName Cell)) Cell
consTable = unsafePerformIO H.new

the :: Eq a => Quad a -> a
the (Quad a b c d)
  | a == b && b == c && c == d  =  a
  | otherwise = error "no 'the', fool"

combine :: Quad Cell -> IO Cell
combine = memoize consTable (\quad -> traverse makeStableName quad) make
  where
    make quad = pure (Cell l quad)
      where
        l = the (level <$> quad) + 1

midY :: Quad a -> Quad a -> Quad a
midY (Quad a b c d) (Quad x y z w) = Quad c d x y

midX :: Quad a -> Quad a -> Quad a
midX (Quad a b c d) (Quad x y z w) = Quad b x d z

midXCell :: Cell -> Cell -> IO Cell
midXCell (Cell _ q1) (Cell _ q2) = combine (midX q1 q2)

midYCell :: Cell -> Cell -> IO Cell
midYCell (Cell _ q1) (Cell _ q2) = combine (midY q1 q2)

{-# NOINLINE calcTable #-}
calcTable :: MemoTable (StableName Cell) Cell
calcTable = unsafePerformIO H.new

-- Result is calculated 2^(l-2) steps in the future
-- Size of 2^(l-1) (ie. half size of the input).
calculate :: Cell -> IO Cell
calculate = memoize calcTable makeStableName calc
  where
    calc cell@(Cell n (Quad a b c d))
      | n == 2 = calcFlat cell -- 4x4
      | n >= 3 = -- >= 8x8
          do north <- midXCell a b
             east <- midYCell b d
             west <- midYCell a c
             south <- midXCell c d
             center <- midXCell west east

             x11 <- calculate a
             x21 <- calculate north
             x31 <- calculate b

             x12 <- calculate west
             x22 <- calculate center
             x32 <- calculate east

             x13 <- calculate c
             x23 <- calculate south
             x33 <- calculate d

             y1 <- calculate =<< combine (Quad x11 x21 x12 x22)
             y2 <- calculate =<< combine (Quad x21 x31 x22 x32)
             y3 <- calculate =<< combine (Quad x12 x22 x13 x23)
             y4 <- calculate =<< combine (Quad x22 x32 x23 x33)
             combine (Quad y1 y2 y3 y4)

    calcFlat cell = do mp <- cellToMap (Pos 0 0) cell
                       let mp' = step mp
                       mapToCell (Pos 1 1) 1 mp'

{-# NOINLINE popTable #-}
popTable :: MemoTable (StableName Cell) Int
popTable = unsafePerformIO H.new

population :: Cell -> IO Int
population = memoize popTable makeStableName pop
  where
    pop (Zero c) = if elem c [O, T, L] then pure 1 else pure 0
    pop (Cell _ quad) = do ps <- traverse population quad
                           return (sum ps)


cellToMap :: Pos -> Cell -> IO Grid
cellToMap p0 (Zero x) = pure $ if elem x [T,L,O] then Map.singleton p0 x else Map.empty
cellToMap p0 cell@(Cell l q) = do
  pop <- population cell
  if pop > 0 then do
    subs <- sequence $ (\cell d -> cellToMap (p0 + d) cell) <$> q <*> dx
    return (fold subs)
    else
    pure Map.empty
  where
    m = 2^(l - 1)
    dx = Quad (Pos 0 0) (Pos m 0) (Pos 0 m) (Pos m m)

blankCell :: Int -> IO Cell
blankCell 0 = pure zX
blankCell l = do sub <- blankCell (l-1)
                 combine (pure sub)

intersectsOrigin :: Pos -> Int -> Bool
intersectsOrigin (Pos x0 y0) l = True --x0 <= 50 && x1 >= 0 && y0 <= 50 && y1 >= 0
  where
    x1 = x0 + n
    y1 = y0 + n
    n = 2^l

mapToCell :: Pos -> Int -> Grid -> IO Cell
mapToCell p0 0 grid = pure (canon (Map.findWithDefault X p0 grid))
mapToCell p0 l grid
  | intersectsOrigin p0 l = combine =<< traverse (\d -> mapToCell (p0+d) (l-1) grid) dx
  | otherwise             = blankCell l
  where
    m = 2^(l - 1)
    dx = Quad (Pos 0 0) (Pos m 0) (Pos 0 m) (Pos m m)

log2 :: Int -> Int
log2 x
  | 2^y == x  = y
  | otherwise = error "not a power of 2"
  where
    y = round (logBase 2 (fromIntegral x))

-- Simulate forward some power-of-two steps in the future.
-- Builds overlapping cells of the corresponding size, then stitches together the outputs.
simulateFast :: Int -> Grid -> IO Grid
simulateFast t grid = do
  in_pieces <- for in_indexes $ \x -> do
    mapToCell x l grid

  out_pieces <- traverse calculate in_pieces
  out_maps <- traverse (\(x, c) -> cellToMap x c) (Map.toList out_pieces)
  return (fold out_maps)
  where
    -- t = 2^(l-2)
    l = log2 t + 2
    xout = 2^(l-1)
    xin = 2^l

    xmin = 0
    xmax = 50
    ymin = 0
    ymax = 50

    -- Blocks of size xin overlapping the input, separated by distance xout so the results do not overlap
    in_indexes = (\p -> p - Pos (xout`div`2) (xout`div`2)) <$> out_indexes
    out_indexes = Map.fromList [(Pos x y, Pos x y) |
                   x <- [xmin, xmin + xout .. xmax],
                   y <- [ymin, ymin + xout .. ymax]]


-- Simulate any arbitrary number of steps into the future by
-- recursively calling simulateFast.
simulateFast' :: Int -> Grid -> IO Grid
simulateFast' 0 state = pure state
simulateFast' t state = do next <- simulateFast t0 state
                           simulateFast' (t - t0) next
  where
    t0 = maximum (takeWhile (<=t) [2^l | l <- [0..]])

-- === ======== === --



decode :: String -> Grid
decode txt = Map.fromList [(Pos x y, dec c)
                          | (y, line) <- zip [0..] (lines txt),
                            (x, c) <- zip [0..] line,
                            elem c (".#|" :: String)]
  where
    dec '.' = O
    dec '#' = L
    dec '|' = T

toChar :: Square -> String
toChar O = "."
toChar T = "|"
toChar L = "#"
toChar X = "\x1b[41m \x1b[m"

display :: Grid -> String
display grid = unlines [ line y | y <- [ymin..ymax]]
  where
    line y = fold [toChar (Map.findWithDefault X (Pos x y) grid) | x <- [xmin..xmax]]
    ymin = minimum (map y (Map.keys grid))
    ymax = maximum (map y (Map.keys grid))
    xmin = minimum (map x (Map.keys grid))
    xmax = maximum (map x (Map.keys grid))

solve1 :: Grid -> Int
solve1 grid = count T final * count L final
  where
    final = Map.elems (iterate step grid !! 10)

main :: IO ()
main = do
  txt <- readFile "input/18.txt"
  let grid0 = decode txt
  putStrLn (display $ iterate step grid0 !! 10)
  print (solve1 grid0)
  grid1 <- simulateFast' 1000000 grid0
  putStrLn (display grid1)

  -- putStrLn (display grid0)
  -- cell0 <- mapToCell (Pos (50-16) (50-16)) 4 grid0
  -- map0 <- cellToMap (Pos 0 0) cell0
  -- putStrLn (display map0)
  -- putStrLn (display $ iterate step map0 !! 4)

  -- cell1 <- calculate cell0
  -- map1 <- cellToMap (Pos 0 0) cell1
  -- putStrLn (display map1)

  -- let drawCell cell = do
  --       mp <- cellToMap 0 cell
  --       putStrLn (display mp)

  -- case cell0 of
  --   Cell _ (Quad a b c d) -> do
  --     north <- midXCell a b
  --     east <- midYCell b d
  --     west <- midYCell a c
  --     south <- midXCell c d
  --     center <- midXCell west east

  --     drawCell a
  --     drawCell north
  --     drawCell b

  --     drawCell west
  --     drawCell center
  --     drawCell east

  --     drawCell c
  --     drawCell south
  --     drawCell d
