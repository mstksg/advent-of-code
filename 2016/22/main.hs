{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Debug.Trace
import Data.Tuple
import Text.Parsec
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H

data P = P !Int !Int deriving (Show, Eq, Ord)

dfline :: Parsec String () (P, Int, Int)
dfline = do string "/dev/grid/node-x"
            x <- number
            string "-y"
            y <- number
            spaces
            size <- number <* char 'T' <* spaces
            used <- number <* char 'T' <* spaces
            avail <- number <* char 'T'
            return (P x y, used, avail)
              where
                number = read <$> many1 digit

pairs' :: [a] -> [(a, a)]
pairs' [] = []
pairs' (a:as) = fmap ((,) a) as ++ pairs' as

pairs :: [a] -> [(a, a)]
pairs xs = pairs' xs ++ fmap swap (pairs' xs)

viable :: (P, Int, Int) -> (P, Int, Int) -> Bool
viable (_, a_used, _) (_, _, b_avail) = a_used > 0 && a_used <= b_avail

input :: String -> [(P, Int, Int)]
input text = case traverse (parse dfline "") ls of
               Right xs -> xs
               Left e -> error (show e)
  where
    ls = drop 2 (lines text)

type Capacity = Map P Int
data Step = Step (Map P Int) P deriving (Show, Eq, Ord)

initial :: [(P, Int, Int)] -> Step
initial nodes = Step mp g
  where
    mp = Map.fromList [(p, used) | (p, used, avail) <- nodes]
    g = snd $ maximum [(x, P x y) | (P x y, _, _) <- nodes, y == 0]

capacity :: [(P, Int, Int)] -> Capacity
capacity nodes = Map.fromList [(p, used + avail) | (p, used, avail) <- nodes]

bounds :: [(P, Int, Int)] -> P
bounds nodes = maximum [p | (p, _, _) <- nodes]

taxicab :: P -> P -> Int
taxicab (P x y) (P a b) = abs (x - a) + abs (y - b)

distance1 :: Step -> Int
distance1 (Step _ p) = taxicab p (P 0 0)

distance2 :: Capacity -> Step -> Int
distance2 cmap (Step used p) = if p == P 0 0 then 0 else taxicab p (P 0 0) + extra
  where
    extra = minimum [taxicab p q - 1 | (q, used_q) <- Map.toList used, fits q used_q, p /= q]
    fits q used_q = used_p + used_q <= cap_q
      where
        used_p = used Map.! p
        cap_q = cmap Map.! q

neighbors :: Capacity -> P -> [P]
neighbors cmap (P x y) = filter exists [P (x+1) y,
                                        P (x-1) y,
                                        P x (y+1),
                                        P x (y-1)]
  where
    exists p = Map.member p cmap

-- constrained :: Capacity -> Int -> P -> [P]
-- constrained cmap gsize p = filter go (neighbors cmap p)
--   where
--     go q = case Map.lookup q cmap of
--              Just cap -> gsize <= cap

load :: String -> (Capacity, Step)
load text = let nodes = input text
            in (capacity nodes, initial nodes)

data Action = Move P P deriving (Show, Eq, Ord)

actions :: Capacity -> Step -> [Action]
actions cmap (Step used g) = [Move p1 p2 | p1 <- to_move, p2 <- neighbors cmap p1, allowed p1 p2]
  where
    to_move = [p | (p, u) <- Map.toList used, u > 0]
    allowed p1 p2 = let used1 = used Map.! p1
                        used2 = used Map.! p2
                        cap2 = cmap Map.! p2
                    in used1 + used2 <= cap2

apply :: Step -> Action -> Step
apply (Step used g) (Move p1 p2) = Step used' (if p1 == g then p2 else g)
  where
    used' = let used_1 = used Map.! p1
            in Map.insert p1 0 $ Map.adjust (+ used_1) p2 used

astar' :: forall a s. (Ord s) => s -> (s -> [a]) -> (s -> a -> Int) -> (s -> a -> s) -> (s -> Int) -> (s -> Maybe String) -> Int
astar' initial actions asize next distance log = go (H.singleton (distance initial, (0, [], initial))) Set.empty
  where
    go :: MinPrioHeap Int (Int, [a], s) -> Set s -> Int
    go frontier visited = case H.view frontier of
      Just ((_, s), frontier') -> let (_, _, ss) = s in
                                    case Set.member ss visited of
                                      True -> go frontier' visited
                                      False -> go' frontier' (Set.insert ss visited) s
      Nothing -> error "out of frontier"

    go' frontier visited (pathsize, path, s)
      | distance s == 0 = pathsize
      | otherwise = go frontier' visited
      where
        nexts = [(pathsize + asize s a + distance s', (pathsize + asize s a, a : path, s'))
                | a <- actions s, let s' = next s a]
        frontier' = frontier `H.union` H.fromList nexts
        !_ = if H.size frontier `mod` 10 == 0 then
               case log s of
                 Just t -> traceShow (pathsize, distance s, H.size frontier, Set.size visited,
                                      fromIntegral (H.size frontier) / fromIntegral (Set.size visited), t) ()
                 Nothing -> ()
             else ()

distance4 :: Capacity -> Step -> Int
distance4 cmap s0
  | p0 == P 0 0 = 0
  | otherwise = minimum [taxicab p1 p0 | Move p1 p2 <- actions cmap s0] + 10 * distance1 s0 - 3
  where
    Step _ p0 = s0

data ZeroStep = Z P P deriving (Show, Eq, Ord)
zero_actions :: Capacity -> ZeroStep -> [Action]
zero_actions cmap (Z g z) = [Move p2 z | p2 <- neighbors cmap z]
zero_asize :: ZeroStep -> Action -> Int
zero_asize _ _ = 1
zero_apply :: ZeroStep -> Action -> ZeroStep
zero_apply (Z g z) (Move p1 p2)
  | p1 == g   = Z p2 p1
  | otherwise = Z g  p1
zero_distance :: ZeroStep -> Int
zero_distance (Z g z)
  | g == P 0 0   = 0
  | zy > 13       = max 1 $ 5 * taxicab g (P 0 0) - 4 + (taxicab z (P 0 13) + taxicab (P 0 13) g) - 1
  | otherwise    = max 1 $ 5 * taxicab g (P 0 0) - 4 + taxicab z g - 1
  where
    P x y = g
    P zx zy = z

distance5 :: Capacity -> Step -> Int
distance5 cmap (Step used g) = zero_distance (Z g z)
  where
    z = snd $ minimum [(u, p) | (p, u) <- Map.toList used]

getG :: Step -> P
getG (Step used g) = g

getZ :: Step -> P
getZ (Step used g) = snd $ minimum [(u, p) | (p, u) <- Map.toList used]

main :: IO ()
main = do
  (cmap, s0) <- load <$> readFile "input.txt"
  let sol = astar' s0 (actions cmap) (\_ _ -> 1) apply (distance5 cmap) (Just . show . getZ)
  print sol
  -- print $ astar' (Z (P 37 0) (P 17 22)) (zero_actions cmap) zero_asize zero_apply zero_distance (Just . show)
