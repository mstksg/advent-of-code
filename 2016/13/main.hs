{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Data.Bits
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Coord = Coord {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Show, Eq, Ord)

passable :: Coord -> Bool
passable (Coord x y) = even (popCount value)
  where
    value = x*x + 3*x + 2*x*y + y + y*y + 1362

initial :: Coord
initial = Coord 1 1

goal :: Coord
goal = Coord 31 39

taxicab :: Coord -> Coord -> Int
taxicab (Coord a b) (Coord c d) = abs (a - c) + abs (b - d)

isGoal :: Coord -> Bool
isGoal c = c == goal

options :: Coord -> [Coord]
options (Coord x y) = do (x', y') <- [(x - 1, y), (x + 1, y),
                                       (x, y - 1), (x, y + 1)]
                         guard (x' >= 0 && y' >= 0)
                         guard (passable (Coord x' y'))
                         return (Coord x' y')

astar :: forall s. Ord s => s -> (s -> [s]) -> (s -> Int) -> Int
astar initial options distance = go (H.singleton (distance initial, (0, initial))) Set.empty
  where
    go :: MinPrioHeap Int (Int, s) -> Set s -> Int
    go frontier visited = case H.view frontier of
      Just ((_, (path, s)), frontier') -> case Set.member s visited of
        True -> go frontier' visited
        False -> go' frontier' visited path s
      Nothing -> error "out of frontier"

    go' frontier visited path s
      | distance s == 0 = path
      | otherwise = go frontier' visited'
      where
        next = [(path + 1 + distance s', (path + 1, s')) | s' <- options s]
        frontier' = frontier `H.union` H.fromList next
        visited' = Set.insert s visited


uniform :: forall s. Ord s => s -> (s -> [s]) -> Int
uniform initial options = go (Seq.singleton (0, initial)) Set.empty
  where
    go :: Seq (Int, s) -> Set s -> Int
    go frontier visited = case Seq.viewl frontier of
      (path, s) Seq.:< frontier' -> case Set.member s visited of
        True -> go frontier' visited
        False -> go' frontier' visited path s
      Seq.EmptyL -> error "out of frontier"

    go' frontier visited path s
      | path > 50 = Set.size visited
      | otherwise = go frontier' visited'
      where
        next = [(path + 1, s') | s' <- options s]
        frontier' = frontier Seq.>< Seq.fromList next
        visited' = Set.insert s visited
