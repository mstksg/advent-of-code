#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.HashTable.IO as H
import           Data.Hashable
import           Data.IORef
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
import           Data.Monoid
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace
import           System.IO.Unsafe
import           System.Mem.StableName
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Util.Util

-- Naive algorithm

input :: String
input = unsafePerformIO (readFile "input/12.txt")

rules :: Map String String
rules = Map.fromList [(i,o) | [i, o] <- map (splitOn " => ") (lines input)]

stringToSet :: Int -> String -> Set Int
stringToSet start xs = Set.fromList [i | (i, c) <- zip [start..] xs, c == '#']

initial :: Set Int
initial = stringToSet 0 (splitOn ": " (head (lines input)) !! 1)

-- Get the neighborhood of each plant.
extend :: Set Int -> Map Int String
extend plants
  | Set.null plants = Map.empty
  | otherwise = Map.fromList [(i, go i) | i <- [Set.findMin plants - 2 .. Set.findMax plants + 2]]
  where
    go i = [if Set.member j plants then '#' else '.' | j <- [i-2 .. i+2]]

-- Step forward the simulation.
step :: Set Int -> Set Int
step plants = Map.keysSet $ Map.filter (\v -> rules Map.! v == "#") $ extend plants

solve1 :: Int
solve1 = sum $ iterate step initial !! 20

-- Hashlife

data Cell = Zero | One | Cell !Int !Cell !Cell
  deriving (Show, Eq)

type MemoTable k v = H.CuckooHashTable k v

{-# NOINLINE zero #-}
zero :: Cell
zero = Zero

{-# NOINLINE one #-}
one :: Cell
one = One

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
level Zero = 0
level One = 0
level (Cell n _ _) = n

{-# NOINLINE consTable #-}
consTable :: MemoTable (StableName Cell, StableName Cell) Cell
consTable = unsafePerformIO H.new

combine :: Cell -> Cell -> IO Cell
combine = curry (memoize consTable (\(l,r) -> (,) <$> makeStableName l <*> makeStableName r) make)
  where
    make (l,r)
      | level l == level r = pure (Cell (level l + 1) l r)
      | otherwise          = error "level mismatch"

slice :: Cell -> (Cell, Cell)
slice (Cell n l r) = (l, r)

cellToString :: Cell -> String
cellToString Zero = "."
cellToString One = "#"
cellToString (Cell _ l r) = cellToString l <> cellToString r

stringToCell :: String -> IO Cell
stringToCell "." = pure zero
stringToCell "#" = pure one
stringToCell xs
  | n == 2^level = do l <- stringToCell (take m xs)
                      r <- stringToCell (drop m xs)
                      combine l r
  where
    n = length xs
    m = n `div` 2
    level = round (logBase 2 (fromIntegral n))

{-# NOINLINE calcTable #-}
calcTable :: MemoTable (StableName Cell) Cell
calcTable = unsafePerformIO H.new

-- Result is calculated 2^(l-3) steps in the future
-- Size of 2^(l-1) (ie. half size of the input).
calculate :: Cell -> IO Cell
calculate = memoize calcTable makeStableName calc
  where
    calc c@(Cell n l r)
      | n == 3 = calcFlat c -- 8
      | n  > 3 = -- 16
          do let (ll, lr) = slice l
                 (rl, rr) = slice r
             mid <- combine lr rl
             x1 <- calculate l
             x2 <- calculate mid
             x3 <- calculate r
             y1 <- calculate =<< combine x1 x2
             y2 <- calculate =<< combine x2 x3
             combine y1 y2

    calcFlat c = let str = cellToString c
                     set = step $ Set.fromList [i | (i, c) <- zip [0..] str, c == '#']
                     str' = [if Set.member i set then '#' else '.' | i <- [2..5]]
                 in stringToCell str'

{-# NOINLINE popTable #-}
popTable :: MemoTable (StableName Cell) Int
popTable = unsafePerformIO H.new

-- Memoized population count (mainly care whether pop is nonzero).
population :: Cell -> IO Int
population = memoize popTable makeStableName pop
  where
    pop Zero = pure 0
    pop One = pure 1
    pop (Cell n l r) = (+) <$> population l <*> population r

log2 :: Int -> Int
log2 x
  | 2^y == x  = y
  | otherwise = error "not a power of 2"
  where
    y = round (logBase 2 (fromIntegral x))

cellToSet :: Int -> Cell -> IO (Set Int)
cellToSet x Zero = pure Set.empty
cellToSet x One = pure (Set.singleton x)
cellToSet x (Cell n l r) = do pl <- population l
                              pr <- population r
                              setl <- if pl > 0 then cellToSet x l else pure Set.empty
                              setr <- if pr > 0 then cellToSet (x + 2^(n-1)) r else pure Set.empty
                              return (Set.union setl setr)

-- Efficiently produce cells with lots of zeros for padding.
zeros :: Int -> IO Cell
zeros 1 = pure zero
zeros n
  | n == 2^log2 n = do x <- zeros (n`div`2)
                       combine x x

-- Build a cell containing this slice of the field
-- position, length
setToCell :: Int -> Int -> Set Int -> IO Cell
setToCell x 1 plants = case Set.member x plants of
                         True -> pure one
                         False -> pure zero
setToCell x size plants
  | Set.null plants = zeros size
  | otherwise       = do let lplants = sliceSet x (div size 2) plants
                             rplants = sliceSet (x + div size 2) (div size 2) plants
                         lcell <- setToCell x (div size 2) lplants
                         rcell <- setToCell (x + div size 2) (div size 2) rplants
                         combine lcell rcell

sliceSet :: Int -> Int -> Set Int -> Set Int
sliceSet x s set = let (_, gx) = Set.split (x-1) set
                       (ls, _) = Set.split (x+s) gx
                   in ls

-- Simulate forward some power-of-two steps in the future.
-- Builds overlapping cells of the corresponding size, then stitches together the outputs.
simulateFast :: Int -> Set Int -> IO (Set Int)
simulateFast t state
  | Set.null state = pure state
  | otherwise = do
      in_pieces <- for in_indexes $ \x -> do
        c <- setToCell x xin state
        seq c (return c)
      out_pieces <- traverse calculate in_pieces
      out_sets <- traverse (\(x, c) -> cellToSet x c) (zip out_indexes out_pieces)
      return (Set.unions out_sets)
  where
    -- t = 2^(l-3)
    l = log2 t + 3
    xout = 2^(l-1)
    xin = 2^l
    smin = Set.findMin state
    smax = Set.findMax state
    -- Blocks of size xin overlapping the input, separated by distance xout so the results do not overlap
    in_indexes = [smin - xin + 1, smin - xin + 1 + xout .. smax - 1]
    out_indexes = [x + div xout 2 | x <- in_indexes]

-- Simulate any arbitrary number of steps into the future by
-- recursively calling simulateFast.
simulateFast' :: Int -> Set Int -> IO (Set Int)
simulateFast' 0 state = pure state
simulateFast' t state = do next <- simulateFast t0 state
                           simulateFast' (t - t0) next
  where
    t0 = maximum (takeWhile (<=t) [2^l | l <- [0..]])

main :: IO ()
main = do print solve1
          print . sum =<< simulateFast' (50 * 10^9) initial
