{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Crypto.Hash
import qualified Data.ByteString.Char8 as C
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H

data Coord = Coord {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Show, Eq, Ord)

data State = State {-# UNPACK #-} !Coord {-# UNPACK #-} !(Context MD5)
data Action = U | D | L | R deriving (Show, Eq, Ord)

actions :: State -> [Action]
actions (State (Coord x y) ctx) = [a | (c, a) <- zip dig [U,D,L,R], elem c "bcdef", legal a]
  where
    dig = show (hashFinalize ctx)
    legal U = y > 0
    legal D = y < 3
    legal L = x > 0
    legal R = x < 3

initial :: String -> State
initial seed = State (Coord 0 0) (hashUpdate hashInit (C.pack seed))

next :: State -> Action -> State
next (State coord hash) d = State (move d coord) hash'
  where
    move U (Coord x y) = Coord x (y-1)
    move D (Coord x y) = Coord x (y+1)
    move L (Coord x y) = Coord (x-1) y
    move R (Coord x y) = Coord (x+1) y
    hash' = hashUpdate hash (C.pack (show d))

taxicab :: Coord -> Coord -> Int
taxicab (Coord a b) (Coord c d) = abs (a - c) + abs (b - d)

distance :: State -> Int
distance (State c _) = taxicab c (Coord 3 3)

astar :: forall a s. s -> (s -> [a]) -> (s -> a -> s) -> (s -> Int) -> [a]
astar initial actions next distance = go (H.singleton (distance initial, (0, [], initial)))
  where
    go :: MinPrioHeap Int (Int, [a], s) -> [a]
    go frontier = case H.view frontier of
      Just ((_, s), frontier') -> go' frontier' s
      Nothing -> error "out of frontier"

    go' frontier (pathsize, path, s)
      | distance s == 0 = reverse path
      | otherwise = go frontier'
      where
        nexts = [(pathsize + 1 + distance s', (pathsize + 1, a : path, s'))
                | a <- actions s, let s' = next s a]
        frontier' = frontier `H.union` H.fromList nexts

solve :: String -> String
solve seed = foldMap show $ astar (initial seed) actions next distance


astar2 :: forall a s. s -> (s -> [a]) -> (s -> a -> s) -> (s -> Int) -> [[a]]
astar2 initial actions next distance = go (H.singleton (distance initial, (0, [], initial)))
  where
    go :: MinPrioHeap Int (Int, [a], s) -> [[a]]
    go frontier = case H.view frontier of
      Just ((_, s), frontier') -> go' frontier' s
      Nothing -> []

    go' frontier (pathsize, path, s)
      | distance s == 0 = reverse path : go frontier
      | otherwise = go frontier'
      where
        nexts = [(pathsize + 1 + distance s', (pathsize + 1, a : path, s'))
                | a <- actions s, let s' = next s a]
        frontier' = frontier `H.union` H.fromList nexts
