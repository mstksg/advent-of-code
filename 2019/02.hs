import           Control.Monad
import           Data.Vector (Vector)
import qualified Data.Vector as V

step :: (Int, Vector Int) -> Maybe (Int, Vector Int)
step (pc, mem) = case mem V.! pc of
  99 -> Nothing
  1 -> Just (pc + 4, mem V.// [(ind (pc + 3), ind (ind (pc + 1)) + ind (ind (pc + 2)))])
  2 -> Just (pc + 4, mem V.// [(ind (pc + 3), ind (ind (pc + 1)) * ind (ind (pc + 2)))])
  o -> error ("invalid opcode: " ++ show o)
  where
    ind i = mem V.! i

run' :: (Int, Vector Int) -> [(Int, Vector Int)]
run' st = case step st of
           Just a -> st : run' a
           Nothing -> [st]

run :: (Int, Vector Int) -> (Int, Vector Int)
run st = last (run' st)

input :: [Int]
input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,19,6,23,2,23,6,27,2,6,27,31,2,13,31,35,1,9,35,39,2,10,39,43,1,6,43,47,1,13,47,51,2,6,51,55,2,55,6,59,1,59,5,63,2,9,63,67,1,5,67,71,2,10,71,75,1,6,75,79,1,79,5,83,2,83,10,87,1,9,87,91,1,5,91,95,1,95,6,99,2,10,99,103,1,5,103,107,1,107,6,111,1,5,111,115,2,115,6,119,1,119,6,123,1,123,10,127,1,127,13,131,1,131,2,135,1,135,5,0,99,2,14,0,0]

configure :: Int -> Int -> (Int, Vector Int)
configure noun verb = (0, V.fromList input V.// [(1, noun), (2, verb)])

solve1 :: Int
solve1 = V.head $ snd $ run (configure 12 2)

solve2 :: [Int]
solve2 = do
  n <- [0..99]
  v <- [0..99]
  guard (V.head (snd $ run (configure n v)) == 19690720)
  return (100 * n + v)
