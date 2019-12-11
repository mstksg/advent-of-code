import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Linear.V2
import           Control.Concurrent

import           Util

input :: IO String
input = readFile "input/10.txt"

parse :: String -> [V2 Int]
parse txt = [V2 x y | (y, line) <- zip [0..] (lines txt),
              (x, c) <- zip [0..] line,
              c == '#'
             ]

dir :: V2 Int -> V2 Int
dir v = (`div` dist v) <$> v

ang :: V2 Int -> Double
ang z = case fromIntegral <$> dir z of
  V2 x y
    | x >= 0 -> atan2 x (-y)
    | x < 0 -> 2*pi + atan2 x (-y)

dist :: V2 Int -> Int
dist (V2 x y)
  | x == 0 || y == 0 = max (abs x) (abs y)
  | otherwise = gcd (abs x) (abs y)

solve1 :: [V2 Int] -> (V2 Int, Int)
solve1 as = (base, detected base)
  where
    detected a = Set.size (Set.fromList [dir (b - a) | b <- as, b /= a])
    base = maximumOn detected as

solve2 :: [V2 Int] -> V2 Int
solve2 as = go g' !! 199
  where
    detected a = Set.size (Set.fromList [dir (b - a) | b <- as, b /= a])
    base = maximumOn detected as
    g = Map.fromListWith (++) [(ang (b - base), [b]) | b <- as, b /= base]
    g' = sortOn (dist . subtract base) <$> g
    go g
      | all null g = []
      | otherwise = foldMap (take 1) g ++ go (drop 1 <$> g)

simulate :: (V2 Int, [V2 Int]) -> IO ()
simulate (base, destroy) = go (Set.fromList destroy) (Set.empty) destroy
  where
    xmin = minimum (map (view _x) destroy)
    xmax = maximum (map (view _x) destroy)
    ymin = minimum (map (view _y) destroy)
    ymax = maximum (map (view _y) destroy)
    go asts destroyed [] = display asts destroyed []
    go asts destroyed (a:as) = display asts destroyed [a] >> go (Set.delete a asts) (Set.insert a destroyed) as
    display asts destroyed a = putStrLn (
      unlines [
          fold [case () of
             () | elem (V2 x y) a -> "\ESC[31mx\ESC[m"
             -- () | Set.member (V2 x y) destroyed -> "x"
             () | Set.member (V2 x y) asts
                  && elem (dir (V2 x y - base)) ((dir . subtract base) <$> a) -> "\ESC[34m#\ESC[m"
             () | Set.member (V2 x y) asts -> "#"
             () | (V2 x y) == base -> "\ESC[32mo\ESC[m"
             () -> "."
          | x <- [xmin..xmax]]
          | y <- [ymin..ymax]]) >> void (getLine)
