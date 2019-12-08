import           Data.List.Split
import           Data.List
import           Data.Char
import           Data.Ord

input :: IO String
input = readFile "input/08.txt"

count x xs = sum [1 | y <- xs, x == y]

minimumOn f xs = minimumBy (comparing f) xs

layers :: String -> [String]
layers txt = chunksOf (25 * 6) (filter isDigit txt)

solve1 :: String -> Int
solve1 txt = (\l -> count '1' l * count '2' l) . minimumOn (count '0') . layers $ txt

solve2 :: String -> IO ()
solve2 txt = putStr . foldMap render . unlines . chunksOf 25 . map resolve . transpose . layers $ txt
  where
    resolve stack = head (dropWhile (=='2') stack ++ "0")
    render '0' = "  "
    render '1' = "\x1b[41m  \x1b[m"
    render x = [x]

main :: IO ()
main = do
  print . solve1 =<< input
  solve2 =<< input
