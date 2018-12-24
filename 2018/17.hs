#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}
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
import           Data.Heap (MinHeap)
import qualified Data.Heap as H
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
import           System.IO.Unsafe
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Util.Util

data Pos = Pos {
  x :: Int,
  y :: Int
  }
  deriving (Show, Eq, Ord)

entry :: (Monad m, CharParsing m) => m (Set Pos)
entry = vertical <|> horizontal
  where
    vertical = do string "x="
                  x <- number
                  string ", y="
                  y1 <- number
                  string ".."
                  y2 <- number
                  return $ Set.fromList [Pos x y | y <- [y1..y2]]
    horizontal = do string "y="
                    y <- number
                    string ", x="
                    x1 <- number
                    string ".."
                    x2 <- number
                    return $ Set.fromList [Pos x y | x <- [x1..x2]]

inputFormat :: (Monad m, CharParsing m) => m (Set Pos)
inputFormat = fold <$> endBy entry spaces

data Info = Info {
  walls :: Set Pos,
  still :: Set Pos,
  reachable :: Set Pos,
  minY :: Int,
  maxY :: Int,
  sources :: Set Pos
  }
  deriving (Show, Eq, Ord)

occupied :: Info -> Pos -> Bool
occupied info p = Set.member p (walls info) || Set.member p (still info)

sliceSet :: Ord a => a -> a -> Set a -> Set a
sliceSet s e xs = case Set.split s xs of
                    (_, ys) -> case Set.split e ys of
                      (zs, _) -> zs

-- Returns the position of the object that blocks our fall, or Nothing if none
fall :: Pos -> Info -> Maybe Pos
fall (Pos x y) info = Set.lookupMin column
  where
    occupied = Set.union (walls info) (still info)
    column = sliceSet (Pos x y) (Pos (x+1) 0) occupied

up :: Pos -> Pos
up (Pos x y) = Pos x (y-1)

down :: Pos -> Pos
down (Pos x y) = Pos x (y+1)

right :: Pos -> Pos
right (Pos x y) = Pos (x+1) y

left :: Pos -> Pos
left (Pos x y) = Pos (x-1) y

enums :: Int -> Int -> [Int]
enums a b = if a < b then [a..b] else [b..a]

line :: Pos -> Pos -> Set Pos
line (Pos x1 y1) (Pos x2 y2)
  | x1 == x2  = Set.fromList [Pos x1 y | y <- enums y1 y2]
  | y1 == y2  = Set.fromList [Pos x y1 | x <- enums x1 x2]
  | otherwise = error "bad line"

dropWater :: Info -> Pos -> Info
dropWater info p
  | occupied info p = info { sources = Set.delete p (sources info) } -- already filled in
  | otherwise       = case fall p info of
                        Just fp -> fillSurface (info { reachable = Set.union (reachable info) (line p (up fp)) }) (up fp) p
                        Nothing -> info { reachable = Set.union (reachable info) (line p p{y=maxY info}) }

simulate :: Info -> Info
simulate info = go (Set.toList (sources info)) info
  where
    go [] info = info
    go (s:ss) info = go ss (dropWater info s)

fillSurface :: Info -> Pos -> Pos -> Info
fillSurface info pstart psource
  | occ (down r) && occ (down l) = -- U shaped, fill with still water
      info { still = still info <> line l r,
             reachable = reachable info <> line l r }
  | otherwise = let info1 = info { reachable = reachable info <> line l r }
                    info2 = if occ (down r) then info1 else info1 { sources = Set.insert (down r) (sources info1) }
                    info3 = if occ (down l) then info2 else info2 { sources = Set.insert (down l) (sources info2) }
                in info3
  where
    r = last $ (pstart:) $ map right $ takeWhile canFlowRight $ iterate right pstart
    l = last $ (pstart:) $ map left $ takeWhile canFlowLeft $ iterate left pstart
    occ p = occupied info p
    canFlowRight p = not (occ (right p)) && occ (down p)
    canFlowLeft p = not (occ (left p)) && occ (down p)

drawInfo :: Info -> String
drawInfo info = unlines [line y | y <- [ymin .. ymax]]
  where
    line y = [draw (Pos x y) | x <- [minX.. maxX]]
    draw p
      | p == Pos 500 0 = '*'
      | Set.member p (walls info) = '#'
      | Set.member p (still info) = '~'
      | Set.member p (reachable info) = '|'
      | otherwise = ' '
    features = walls info <> reachable info <> still info
    minX = minimum (map x $ toList features)
    maxX = maximum (map x $ toList features)
    ymin = max 0 ((maximum $ (0:) $ map y $ toList $ reachable info) - 200)
    ymax = ymin + 200

main :: IO ()
main = do
  txt <- readFile "input/17.txt"
  let input = parse inputFormat txt
      info = Info { walls = input,
                    still = Set.empty,
                    reachable = Set.empty,
                    minY = minimum (y <$> toList input),
                    maxY = maximum (y <$> toList input),
                    sources = Set.singleton (Pos 500 0)
                  }
      go info = do
        -- print (Set.size (reachable info))
        T.putStrLn (T.pack $ drawInfo info)
        -- threadDelay (100 * 10^3)
        let info' = (simulate info)
        if info' /= info then
          go info'
          else do
          print (length [Pos x y | Pos x y <- toList (reachable info), y >= minY info, y <= maxY info])
          print (Set.size (still info))

  go info
