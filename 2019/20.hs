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

data Maze = Maze {
  mfloor :: Set Pos,
  mportals :: Map Pos Text,
  mportalp :: Map Text [Pos]
  }
  deriving (Show)

inner :: V2 Int -> Bool
inner (V2 x y) = and [x >= 10,
                      y >= 10,
                      x <= 100,
                      y <= 100]

parseInput :: String -> Maze
parseInput txt = Maze{..}
  where
    ls = lines txt
    mfloor = Set.fromList [V2 x y
                          | (y, line) <- zip [0..] ls,
                            (x, c) <- zip [0..] line,
                            c == '.']

    lookup x y
      | y >= 0 && x >= 0 && y < length ls && x < length line = Just (line !! x)
      | otherwise = Nothing
      where
        line = ls !! y

    horizontal x y = case (lookup x y, lookup (x+1) y) of
      (Just c1, Just c2) | isUpper c1 && isUpper c2 -> Just (T.pack [c1, c2])
      _ -> Nothing

    vertical x y = case (lookup x y, lookup x (y+1)) of
      (Just c1, Just c2) | isUpper c1 && isUpper c2 -> Just (T.pack [c1, c2])
      _ -> Nothing

    mportals = Map.fromList $ do
      (y, line) <- zip [0..] ls
      (x, c) <- zip [0..] line
      case (horizontal x y, vertical x y) of
        (Just l, Nothing)
          | lookup (x-1) y == Just '.' -> return (V2 (x-1) y, l)
          | lookup (x+2) y == Just '.' -> return (V2 (x+2) y, l)
        (Nothing, Just l)
          | lookup x (y-1) == Just '.' -> return (V2 x (y-1), l)
          | lookup x (y+2) == Just '.' -> return (V2 x (y+2), l)
        _ -> mzero

    mportalp = Map.fromListWith (++) $ do
      (p, l) <- Map.toList mportals
      return (l, [p])

neighbors :: V2 Int -> [V2 Int]
neighbors p = [p + V2 dx dy | dx <- [-1, 0, 1], dy <- [-1, 0, 1],
               abs dx + abs dy == 1]

solve1 :: Maze -> Int
solve1 Maze{..} = bfs actions goal (head $ mportalp Map.! "AA")
  where
    actions p = [n | n <- neighbors p, Set.member n mfloor] ++
                [k | Just label <- [Map.lookup p mportals],
                  Just ks <- [Map.lookup label mportalp],
                  k <- ks,
                  k /= p]

    goal p = case Map.lookup p mportals of
      Just "ZZ" -> True
      _ -> False

solve2 :: Maze -> Int
solve2 Maze{..} = bfs actions goal (head (mportalp Map.! "AA"), 0)
  where
    actions (p, level) = step ++ portal
      where
        step = [(n, level) | n <- neighbors p, Set.member n mfloor]
        portal = do
          Just label <- [Map.lookup p mportals]
          Just ks <- [Map.lookup label mportalp]
          k <- ks
          guard (k /= p)
          case level of
            0 | not (inner p) -> mzero
            _ | inner p -> [(k, level+1)]
            _ | not (inner p) -> [(k, level-1)]

    goal (p, level) = case Map.lookup p mportals of
      Just "ZZ" -> level == 0
      _ -> False
