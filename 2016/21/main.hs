import Control.Applicative
import Text.Parsec hiding ((<|>))
import Data.List

replace :: Int -> a -> [a] -> [a]
replace 0 x (y:ys) = x:ys
replace n x (y:ys) = y : replace (n-1) x ys

data Operation a = SwapPosition Int Int
                 | SwapLetters a a
                 | RotateLeft Int
                 | RotateRight Int
                 | RotateLetter a
                 | ReversePart Int Int
                 | MovePosition Int Int
                 deriving (Show, Eq, Ord)

swapPosition :: Int -> Int -> [a] -> [a]
swapPosition x y str = replace x cy $ replace y cx str
  where
    cx = str !! x
    cy = str !! y

swapLetters :: Eq a => a -> a -> [a] -> [a]
swapLetters x y str = map go str
  where
    go c
      | c == x    = y
      | c == y    = x
      | otherwise = c

rotateLeft :: Int -> [a] -> [a]
rotateLeft n str = take size (drop n (cycle str))
  where
    size = length str

rotateRight :: Int -> [a] -> [a]
rotateRight n str = take size (drop (size - n `mod` size) (cycle str))
  where
    size = length str

rotateLetter :: Eq a => a -> [a] -> [a]
rotateLetter c str = case elemIndex c str of
                       Just n
                         | n >= 4    -> rotateRight (n + 2) str
                         | otherwise -> rotateRight (n + 1) str
                       Nothing -> str

reversePart :: Int -> Int -> [a] -> [a]
reversePart a b str = part1 ++ reverse part2 ++ part3
  where
    part1 = take a str
    part2 = take (b - a + 1) (drop a str)
    part3 = drop (b + 1) str

movePosition :: Int -> Int -> [a] -> [a]
movePosition x y str = take y str' ++ [cx] ++ drop y str'
  where
    cx = str !! x
    str' = take x str ++ drop (x+1) str

alt :: Alternative f => [f a] -> f a
alt = foldr (<|>) empty

operation :: Parsec String () (Operation Char)
operation = alt $ map try [
  do string "swap "
     alt [do string "position "
             x <- number
             string " with position "
             y <- number
             return (SwapPosition x y),
          do string "letter "
             x <- letter
             string " with letter "
             y <- letter
             return (SwapLetters x y)
         ],
  do string "rotate "
     alt [do string "left "
             n <- number
             string " step"
             return (RotateLeft n),
          do string "right "
             n <- number
             string " step"
             return (RotateRight n),
          do string "based on position of letter "
             c <- letter
             return (RotateLetter c)
         ],
  do string "reverse positions "
     x <- number
     string " through "
     y <- number
     return (ReversePart x y),
  do string "move position "
     x <- number
     string " to position "
     y <- number
     return (MovePosition x y)
  ]
  where
    number = read <$> many1 digit

forward :: Eq a => Operation a -> [a] -> [a]
forward (SwapPosition x y) = swapPosition x y
forward (SwapLetters x y) = swapLetters x y
forward (RotateLeft n) = rotateLeft n
forward (RotateRight n) = rotateRight n
forward (RotateLetter c) = rotateLetter c
forward (ReversePart x y) = reversePart x y
forward (MovePosition x y) = movePosition x y

back :: Eq a => Operation a -> [a] -> [a]
back (SwapPosition x y) = swapPosition x y
back (SwapLetters x y) = swapLetters x y
back (RotateLeft n) = rotateRight n
back (RotateRight n) = rotateLeft n
back (RotateLetter c) = \s ->
                          head [r | n <- [0..length s],
                                 let r = rotateLeft n s,
                                     rotateLetter c r == s]
back (ReversePart x y) = reversePart x y
back (MovePosition x y) = movePosition y x

parseOp :: String -> (Operation Char)
parseOp line = case parse operation "" line of
                 Right f -> f
                 Left e -> error (show e)

part1 :: String -> (String -> String)
part1 text s = foldl (\s f -> f s) s ops
  where
    ops = map (forward . parseOp) (lines text)

part2 :: String -> (String -> String)
part2 text s = foldr id s ops
  where
    ops = map (back . parseOp) (lines text)
