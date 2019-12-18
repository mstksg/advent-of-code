{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Foldable
import qualified Data.Graph.Inductive.Graph as FGL
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.MST as FGL
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Debug.Trace
import           Linear.V2
import           System.IO.Unsafe

import           AStar
import           Util

type Pos = V2 Int
type Grid = Map Pos Tile

data Tile = Start | Floor | Wall | Key Char | Door Char
  deriving (Show, Eq, Ord)

isPassable _ Start = True
isPassable _ Floor = True
isPassable _ (Key c) = True
isPassable h (Door c) = Set.member c h
isPassable _ Wall = False

input :: Grid
input = unsafePerformIO $ do
  txt <- readFile "input/18.txt"
  return (parseInput txt)

neighbors :: V2 Int -> [V2 Int]
neighbors p = [p + V2 dx dy | dx <- [-1, 0, 1], dy <- [-1, 0, 1],
               abs dx + abs dy == 1]

steps :: Grid -> Set Char -> Pos -> [Pos]
steps grid keysHeld p = filter (isPassable keysHeld . (grid Map.!)) (neighbors p)

parseInput :: String -> Map Pos Tile
parseInput txt = Map.fromList [(V2 x y, case c of
                                   '.' -> Floor
                                   '@' -> Start
                                   '#' -> Wall
                                   _ | isUpper c -> Door (toLower c)
                                   _ | isLower c -> Key c
                                   ) | (y, line) <- zip [0..] (lines txt),
                               (x, c) <- zip [0..] line]

solve1 :: Grid -> Int
solve1 grid = astar actions dist goal (Set.empty, origin)
  where
    pc = precompute grid
    allKeys = Set.fromList [c | Key c <- toList grid]
    keyPositions = Map.fromList [(c, p) | (p, Key c) <- Map.toList grid]
    actions (keys, pos) = do
      let reachable = (astarAll
                       (steps grid keys)
                       (\_ _ -> 1)
                       (\p -> case grid Map.! p of
                           Key c | not (Set.member c keys) -> 0
                           _ -> 1)
                        pos) :: [(Pos, Int)]
      (p, d) <- reachable
      case grid Map.! p of
        Key c -> return (Set.insert c keys, p)
      -- next <- steps grid keys pos
      -- return (case grid Map.! next of
      --           Key c -> Set.insert c keys
      --           _ -> keys, next)

    dist (keys1, pos1) (keys2, pos2) =
      case grid Map.! pos2 of
        Key c -> (astar
                  (steps grid keys1)
                  (\_ _ -> 1)
                  (\p -> pc Map.! c Map.! p)
                  pos1) :: Int

    goal (keys, pos)
      | Set.null remaining = 0
      | otherwise = let e = estimate pos rmap in
                      traceShow (keys, pos, e) e
        -- minimum [pc Map.! rk Map.! pos | rk <- toList remaining]
        --            + Set.size remaining - 1
      where
        remaining = Set.difference allKeys keys
        rmap = Map.fromList [(keyPositions Map.! c, pc Map.! c) | c <- Set.toList remaining]

    origin = head [p | (p, Start) <- Map.toList grid]

precompute :: Grid -> Map Char (Map Pos Int)
precompute grid = Map.fromList $ do
  let allKeys = Set.fromList [c | Key c <- toList grid]
  (keyp, Key keyc) <- Map.toList grid
  let actions p = steps grid allKeys p
  return (keyc, Map.fromList $ exploreOn id actions keyp)

estimate :: Pos -> Map Pos (Map Pos Int) -> Int
estimate pos remaining
  | Map.null remaining = 0
  | otherwise = sum [head [len | (_, len) <- path] | FGL.LP path <- FGL.msTree gr]
  where
    d a b | a == pos = remaining Map.! b Map.! a
    d a b | otherwise = remaining Map.! a Map.! b
    nodes = (pos : Map.keys remaining)
    posId = Map.fromList (zip nodes [1..])
    gr = (FGL.mkGraph
          [(posId Map.! p, p) | p <- nodes]
          [(posId Map.! a, posId Map.! b, d a b) | a <- nodes, b <- nodes, a /= b]
           :: Gr Pos Int)

data S = S (Set Char) [Pos]
  deriving (Show, Eq, Ord)

solve2 :: Grid -> Int
solve2 grid = (astarOn fst
               (actions.fst)
               (\_ (_,d) -> d)
               (goal.fst)
               (S Set.empty newstarts, 0))
  where
    allKeys = Set.fromList [c | Key c <- toList grid]
    actions (S keys pos4) = do
      i <- [0..3]
      let keySet = keySets !! i
          remaining = Set.difference keySet keys
      guard (Set.size remaining > 0)
      let pos = pos4 !! i
      let reachable = (astarAll
                       (steps grid' keys)
                       (\_ _ -> 1)
                       (\p -> case grid Map.! p of
                           Key c | not (Set.member c keys) -> 0
                           _ -> 1)
                        pos) :: [(Pos, Int)]
      (next, dist) <- reachable
      let keys' = case grid' Map.! next of
                    Key c -> Set.insert c keys
                    _ -> keys
      return (S keys' ((ix i .~ next) pos4), dist)
    pc = precompute grid'
    goal (S keys pos4) = let ee = sum $ do
                              i <- [0..3]
                              let keySet = keySets !! i
                                  pos = pos4 !! i
                                  remaining = Set.difference keySet keys
                                  rmap = Map.fromList [(keyLocation rc, pc Map.! rc)
                                                      | rc <- Set.toList remaining]
                                  e = estimate pos rmap
                              return e
                        in traceShow (keys, pos4, ee) ee

    keyLocation :: Char -> Pos
    keyLocation = let lmap = Map.fromList [(c, p) | (p, Key c) <- Map.toList grid]
                  in \c -> lmap Map.! c

    keySets :: [Set Char]
    keySets = do
      o <- newstarts
      return $ Set.fromList $ do
        (p, Key c) <- Map.toList grid
        guard (Map.member o (pc Map.! c))
        return c

    origin = head [p | (p, Start) <- Map.toList grid]
    newstarts = [origin + d | d <- [V2 (-1) (-1),
                                    V2 (-1) 1,
                                    V2 1 (-1),
                                    V2 1 1]]
    grid' = Map.unionWith (const id) grid (
      Map.fromList (
          [(p, Wall) | p <- origin : neighbors origin]
          ++ [(p, Start) | p <- newstarts]))

main :: IO ()
main = do
  print (solve1 input)
  print (solve2 input)
