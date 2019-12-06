{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}

import           Criterion
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           AStar

parseOrbits :: String -> Map String String
parseOrbits txt = Map.fromList [(b, a) | [a, b] <- map (splitOn ")") (lines txt)]

countOrbits :: Map String String -> Map String Int
countOrbits orb = ct
  where
    ct = fmap (\v -> Map.findWithDefault 0 v ct + 1) orb

solve1 :: String -> Int
solve1 txt = sum $ countOrbits $ parseOrbits txt

solve2 :: String -> Int
solve2 txt = bfs (\p -> step Map.! p) (== (orb Map.! "SAN")) (orb Map.! "YOU")
  where
    orb = parseOrbits txt
    rev = Map.fromListWith (++) [(b, [a]) | (a, b) <- Map.toList orb]
    step = Map.unionWith (++) (pure <$> orb) rev

main :: IO ()
main = do
  txt <- readFile "input/06.txt"
  benchmark (nf solve1 txt)
  benchmark (nf solve2 txt)
