import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int,Int)

step 'U' (x,y) = (x, y+1)
step 'D' (x,y) = (x, y-1)
step 'L' (x,y) = (x-1, y)
step 'R' (x,y) = (x+1, y)

parseLine :: String -> Map Pos Int
parseLine line = go (splitOn "," line) (0,0) 0
  where
    go :: [String] -> Pos -> Int -> Map Pos Int
    go [] p l = Map.singleton p l
    go ((d:dist):parts) p l =
      let segment = take (read dist + 1) (iterate (step d) p)
      in Map.unionWith min (Map.fromList (zip segment [l..])) (go parts (last segment) (l + read dist))

manhatten :: Pos -> Int
manhatten (x,y) = abs x + abs y

solve1 :: String -> Int
solve1 input = minimum [manhatten k | (k, v) <- Map.toList occ, v > 1, k /= (0,0)]
  where
    occ = Map.unionsWith (+) [const 1 <$> parseLine line | line <- lines input]

solve2 :: String -> Int
solve2 input = minimum [sum v | (k, v) <- Map.toList occ, length v > 1, k /= (0,0)]
  where
    occ = Map.unionsWith (++) [pure <$> parseLine line | line <- lines input]
