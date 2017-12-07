#!/usr/bin/runhaskell
{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Data.Foldable
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U

type Program = String

data Info = Info {
  programWeight :: Map Program Int,
  programChildren :: Map Program [Program]
  }
  deriving (Show)

parseWeight :: String -> Int
parseWeight = read . init . tail

parseChild :: String -> Program
parseChild = filter (/= ',')

parseTower :: String -> Info
parseTower text = Info (Map.unions (map programWeight infos)) (Map.unions (map programChildren infos))
  where
    go line = case words line of
                [name, sw] -> Info (Map.singleton name (parseWeight sw)) Map.empty
                (name : sw : "->" : xs) -> Info (Map.singleton name (parseWeight sw))
                                           (Map.singleton name (map parseChild xs))
    infos = map go (lines text)

getPrograms :: Info -> Set Program
getPrograms (Info ws pc) = Map.keysSet ws

getParents :: Info -> Map Program Program
getParents (Info ws pc) = Map.unions $ do
  (p, cs) <- Map.toList pc
  c <- cs
  return (Map.singleton c p)

data Tower = Tower { name :: Program,
                     localWeight :: Int,
                     totalWeight :: Int,
                     subTowers :: [Tower] }
  deriving (Show, Eq)

makeTower :: Map Program Int -> Map Program [Program] -> Program -> Tower
makeTower weights children root = Tower root w (w + sum (map totalWeight sub)) sub
  where
    Just w = Map.lookup root weights
    sub = case Map.lookup root children of
            Just names -> map (makeTower weights children) names
            Nothing -> []

majority :: Ord a => [a] -> a
majority xs = sort xs !! (length xs `div` 2)

wrongWeight :: Tower -> [(Program, Int, Int, [Int])]
wrongWeight t = let expect = majority (map totalWeight (subTowers t))
                    [wrong] = filter (\t -> totalWeight t /= expect) (subTowers t)
                in go expect wrong
  where
    go expected t = case subTowers t of
                      [] -> [pathPiece]
                      xs -> let esub = majority (map totalWeight xs)
                                wrong = filter (\t -> totalWeight t /= esub) xs
                            in case wrong of
                                 [] -> [pathPiece]
                                 [x] -> pathPiece : go esub x
      where
        subWeight = sum (map totalWeight (subTowers t))
        pathPiece = (name t, totalWeight t - subWeight, expected - subWeight, map totalWeight (subTowers t))

main :: IO ()
main = do
  inf@(Info weights children) <- parseTower <$> readFile "input.07"
  let programs = getPrograms inf
      parents = getParents inf
      root = head [p | p <- Set.toList programs, Nothing == Map.lookup p parents]
      tower = makeTower weights children root
  print (name tower)
  print (map totalWeight (subTowers tower))
  print (totalWeight tower)
  print (wrongWeight tower)
