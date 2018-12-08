#!/usr/bin/env stack
-- stack runghc
import           Data.Monoid
import           Data.Foldable
import           Control.Monad
import           System.IO.Unsafe
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Parsec as Parsec

input :: String
input = unsafePerformIO (readFile "input/3.txt")

data Rect = Rect Int Int Int Int
  deriving (Show)

claims :: Map Int Rect
claims = either (error.show) id (Parsec.parse p "input/3.txt" input)
  where
    p = Map.fromList <$> endBy claim spaces
    number = read <$> some digit
    claim = do string "#"
               idd <- number
               string " @ "
               x <- number
               string ","
               y <- number
               string ": "
               w <- number
               string "x"
               h <- number
               return (idd, Rect x y w h)

collide :: (Int, Int) -> Rect -> Bool
collide (x, y) (Rect x1 y1 w h) = x1 <= x && x < x2 && y1 <= y && y < y2
  where
    x2 = x1 + w
    y2 = y1 + h

boundLine :: (Int, Int) -> (Int, Int) -> (Int, Int)
boundLine (x, w) (y, h) = (min x y, max (x + w) (y + h) - min x y)

bounding :: Rect -> Rect -> Rect
bounding (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) = Rect x3 y3 w3 h3
  where
    (x3, w3) = boundLine (x1, w1) (x2, w2)
    (y3, h3) = boundLine (y1, h1) (y2, h2)

cells :: Rect -> [(Int, Int)]
cells (Rect x y w h) = do
  px <- [x..x+w-1]
  py <- [y..y+h-1]
  return (px, py)

solve1 = length $ do
  (x,y) <- cells whole
  guard (length (uses x y) >= 2)
  return (x, y)
  where
    whole = foldr1 bounding claims
    uses x y = [i | (i, rect) <- Map.toList claims, collide (x,y) rect]

intersectsLine :: (Int,Int) -> (Int,Int) -> Bool
intersectsLine (x1,w1) (x2,w2) = max x1 x2 < min (x1 + w1) (x2 + w2)

intersects :: Rect -> Rect -> Bool
intersects (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  intersectsLine (x1,w1) (x2,w2) &&
  intersectsLine (y1,h1) (y2,h2)

solve2 = do
  (x, c) <- Map.toList claims
  let is = [d | d <- Map.elems claims, intersects c d]
  guard (length is == 1)
  return (x, c)

main :: IO ()
main = do
  -- print solve1
  print solve2
