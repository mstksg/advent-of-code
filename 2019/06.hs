import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
import           Data.Heap (MinHeap)
import qualified Data.Heap as H
import           Data.Set (Set)
import qualified Data.Set as Set

parseOrbits :: String -> Map String String
parseOrbits txt = Map.fromList [(b, a) | [a, b] <- map (splitOn ")") (lines txt)]

countOrbits :: Map String String -> Map String Int
countOrbits orb = ct
  where
    ct = fmap (\v -> Map.findWithDefault 0 v ct + 1) orb

solve1 :: String -> Int
solve1 txt = sum $ countOrbits $ parseOrbits txt

solve2 :: String -> Int
solve2 txt = astar (\p -> step Map.! p) (\_ _ -> 1) (\p -> if p == (orb Map.! "SAN") then 0 else 1) (orb Map.! "YOU")
  where
    orb = parseOrbits txt
    rev = Map.fromListWith (++) [(b, [a]) | (a, b) <- Map.toList orb]
    step = Map.unionWith (++) (pure <$> orb) rev

-- Pasted from last year :)
astar :: forall s. (Ord s) => (s -> [s]) -> (s -> s -> Int) -> (s -> Int) -> s -> Int
astar actions cost goal s0 = go Set.empty (H.singleton (goal s0, (s0, 0)))
  where
    go :: Set s -> MinHeap (Int, (s, Int)) -> Int
    go visited frontier =
      case H.view frontier of
        Just ((_, (s, d)), frontier')
          | Set.member s visited -> go visited frontier'
          | goal s == 0 ->  d
          | otherwise   ->  let new = H.fromList [let d' = d + cost s s'
                                                  in (d' + goal s', (s', d'))
                                                 | s' <- actions s]
                            in go visited' (frontier' <> new)
            where
              visited' = Set.insert s visited
        Nothing -> error "no route"
