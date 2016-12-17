{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Debug.Trace
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Fuel = Int

data Item = Chip {-# UNPACK #-} !Fuel | Generator {-# UNPACK #-} !Fuel
          deriving (Show, Eq, Ord)

data Facility = Facility ![ItemSet] {-# UNPACK #-} !Int
              deriving (Show, Eq, Ord)

data Direction = Up | Down deriving (Show, Eq, Ord)
data Carry = One !Item | Two !Item !Item deriving (Show, Eq, Ord)
data Action = Action !Direction !Carry deriving (Show, Eq, Ord)

type ItemSet = IntSet

itemsList :: ItemSet -> [Item]
itemsList items = map go (IntSet.toList items)
  where
    go n
      | n > 0 = Chip n
      | otherwise = Generator (-n)

itemsSet :: [Item] -> ItemSet
itemsSet items = IntSet.fromList (map go items)
  where
    go (Chip n) = n
    go (Generator n) = -n

itemMember :: Item -> ItemSet -> Bool
itemMember (Chip n) = IntSet.member n
itemMember (Generator n) = IntSet.member (-n)

initial :: Facility
initial = Facility items 0
  where
    items = map itemsSet [
      -- [Generator "thulium", Chip "thulium", Generator "plutonium", Generator "strontium"] <> [Chip "e", Chip "d", Generator "e", Generator "d"],
      -- [Chip "plutonium", Chip "strontium"],
      -- [Generator "promethium", Chip "promethium", Generator "ruthenium", Chip "ruthenium"],
      [Generator 1, Chip 1, Generator 2, Generator 3] <> [Chip 6, Chip 7, Generator 6, Generator 7],
      [Chip 2, Chip 3],
      [Generator 4, Chip 4, Generator 5, Chip 5],
      []
      ]

elevate :: Direction -> Int -> Int
elevate Up 3 = 3
elevate Up n = n + 1
elevate Down 0 = 0
elevate Down n = n - 1

setList :: Int -> a -> [a] -> [a]
setList 0 a (x:xs) = a:xs
setList n a (x:xs) = x : setList (n-1) a xs

chooseItems :: [Item] -> [Carry]
chooseItems xs = (map One xs) ++ chooseTwo xs
  where
    chooseTwo [] = []
    chooseTwo (x:xs) = map (Two x) xs ++ chooseTwo xs

allActions :: Facility -> [Action]
allActions (Facility items floor) = do
  d <- [Up, Down]
  guard (not (d == Up && floor == 3))
  guard (not (d == Down && floor == 0))
  carry <- chooseItems (itemsList (items !! floor))
  return (Action d carry)

nextStates :: Facility -> [Facility]
nextStates s = do act <- allActions s
                  let next = move act s
                  guard (isSafe next)
                  return next

isSafe :: Facility -> Bool
isSafe (Facility items _) = all safe items
  where
    safe items = all hasGenerator (itemsList items) || all isChip (itemsList items)
      where
        hasGenerator (Generator _) = True
        hasGenerator (Chip c) = itemMember (Generator c) items
        isChip (Generator _) = False
        isChip (Chip _) = True

move :: Action -> Facility -> Facility
move (Action d c) (Facility items floor)
  | nextFloor == floor  = (Facility items floor)
  | otherwise           = (Facility items' nextFloor)
  where
    nextFloor = elevate d floor
    carrySet = case c of
      One item -> itemsSet [item]
      Two i1 i2 -> itemsSet [i1, i2]
    itemsFrom' = IntSet.difference (items !! floor) carrySet
    itemsNext' = IntSet.union (items !! nextFloor) carrySet
    items' = setList floor itemsFrom' $ setList nextFloor itemsNext' $ items

facilityGoal :: Facility -> Bool
facilityGoal (Facility items _) = all IntSet.null (take 3 items)

-- solve :: Ord s => s -> (s -> [s]) -> (s -> Bool) -> [s]
-- solve initial options goal = go (Seq.singleton ([], initial)) Set.empty
--   where
--     go frontier visited
--       | goal s = reverse (s : path)
--       | Set.member s visited = go frontierTail visited
--       | otherwise = trc $ go frontier' visited'
--       where
--         (path, s) Seq.:< frontierTail = Seq.viewl frontier
--         options' = filter (\s' -> not (Set.member s' visited)) (options s)
--         frontier' = frontierTail Seq.>< Seq.fromList [(s:path,  s') | s' <- options']
--         visited' = Set.insert s visited

--         trc = if Set.size visited `mod` 10 == 0 then
--                 traceShow (Seq.length frontier, Set.size visited)
--               else id

solve :: Ord s => s -> (s -> [s]) -> (s -> Bool) -> Int
solve initial options goal = go (Seq.singleton (0, initial)) Set.empty
  where
    go frontier visited
      | goal s = path
      | Set.member s visited = go frontierTail visited
      | otherwise = trc $ ((go $! frontier') $! visited')
      where
        (!path, s) Seq.:< frontierTail = Seq.viewl frontier
        options' = filter (\s' -> not (Set.member s' visited)) (options s)
        frontier' = frontierTail Seq.>< Seq.fromList [(path+1,  s') | s' <- options']
        visited' = Set.insert s visited

        trc = if Set.size visited `mod` 10 == 0 then
                traceShow (path, Seq.length frontier, Set.size visited)
              else id

main :: IO ()
main = print $ solve initial nextStates facilityGoal
