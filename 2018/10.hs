#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           System.IO.Unsafe
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

trim = dropWhile isSpace . dropWhileEnd isSpace

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () c -> s -> c
parse p s = either (error.show) id (Parsec.parse p "" s)

input :: String
input = unsafePerformIO (readFile "input/10.txt")

data V2 a = V2 a a
  deriving (Show, Eq, Ord, Functor)

instance Applicative V2 where
  pure a = V2 a a
  (V2 f g) <*> (V2 a b) = V2 (f a) (g b)

instance Monad V2 where
  return = pure
  (V2 a b) >>= f = case (f a, f b) of
                     (V2 a' _, V2 _ b') -> V2 a' b'

instance Num a => Num (V2 a) where
  fromInteger n = pure (fromInteger n)
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  signum = undefined
  abs = undefined

norm2 :: Num a => V2 a -> a
norm2 (V2 a b) = a*a + b*b

norm :: Integral a => V2 a -> Double
norm v = sqrt (fromIntegral $ norm2 v)

num :: CharParsing p => p Int
num = read <$> (some (digit <|> char '-'))

points :: [(V2 Int, V2 Int)]
points = parse (endBy p spaces) input
  where
    p = do
      string "position="
      p1 <- pair
      string "velocity="
      p2 <- pair
      return (p1, p2)
    pair = do
      char '<'
      spaces
      x <- num
      char ','
      spaces
      y <- num
      char '>'
      spaces
      return (V2 x y)

average xs = sum xs / fromIntegral (length xs)

step :: Int -> [(V2 Int, V2 Int)] -> [(V2 Int, V2 Int)]
step n = map (\(p, v) -> (p + pure n * v, v))

adjacency :: [(V2 Int, V2 Int)] -> Int
adjacency xs = sum [1 | (x1, _) <- xs, (x2, _) <- xs, norm2 (x1 - x2) == 1]

draw :: [(V2 Int, V2 Int)] -> IO ()
draw xs = putStrLn $ unlines ["|" ++ line y ++ "|" | y <- [y1..y2]]
  where
    (V2 x1 y1, V2 x2 y2) = bbox xs
    xset = Set.fromList [x | (x, v) <- xs]
    line y = fold [if Set.member (V2 x y) xset then "##" else "  " | x <- [x1..x2]]

bbox :: [(V2 Int, V2 Int)]-> (V2 Int, V2 Int)
bbox points = (foldr1 (liftA2 min) (map fst points),
               foldr1 (liftA2 max) (map fst points))

maximumOn f = maximumBy (comparing f)

solve2 :: Int
solve2 = maximumOn (\i -> adjacency (step i points)) [round t - 5 .. round t + 5]
  where
    t = average [norm p / norm v | (p, v) <- points]

solve1 :: IO ()
solve1 = draw (step solve2 points)

main :: IO ()
main = do
  solve1
  print solve2
