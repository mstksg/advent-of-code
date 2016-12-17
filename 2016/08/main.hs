import Data.List
import Data.Foldable
import Text.Parsec

data Command = Rect Int Int
             | RotateRow Int Int
             | RotateColumn Int Int
             deriving (Show)

command :: Parsec String () Command
command = try rect <|> try row <|> column
  where
    number = read <$> many1 digit
    rect = do string "rect "
              x <- number
              string "x"
              y <- number
              return (Rect x y)
    row = do string "rotate row y="
             y <- number
             string " by "
             n <- number
             return (RotateRow y n)
    column = do string "rotate column x="
                y <- number
                string " by "
                n <- number
                return (RotateColumn y n)

parsed :: String -> Command
parsed line = case parse command "" line of
                Right c -> c
                x -> error (show x)

type Screen = [[Bool]]

initial :: Screen
initial = replicate 6 (replicate 50 False)

display :: Screen -> IO ()
display xs = putStr $ unlines (map (map d) xs)
  where
    d True = '#'
    d False = ' '

rect :: Int -> Int -> Screen -> Screen
rect x y screen = map go (take y screen) ++ drop y screen
  where
    go line = replicate x True ++ drop x line

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow y n screen = take y screen ++ [go (screen !! y)] ++ drop (y+1) screen
  where
    go row = take width (drop (width - n) (cycle row))
    width = 50

rotateColumn :: Int -> Int -> Screen -> Screen
rotateColumn x n screen = transpose flipped'
  where
    flipped = transpose screen
    flipped' = take x flipped ++ [go (flipped !! x)] ++ drop (x+1) flipped
    go column = take height (drop (height - n) (cycle column))
    height = 6

apply :: Command -> Screen -> Screen
apply (Rect x y) = rect x y
apply (RotateRow y n) = rotateRow y n
apply (RotateColumn x n) = rotateColumn x n

part1Screen :: String -> Screen
part1Screen text = final
  where
    commands = map parsed (lines text)
    final = foldl (flip apply) initial commands

part1 :: String -> Int
part1 text = length $ filter id $ fold final
  where
    final = part1Screen text

text :: IO String
text = readFile "input.txt"
