import Data.Monoid

data Step = Step Int Int deriving (Show)
data Button = Button Int Int deriving (Show)

valid :: Button -> Bool
valid (Button a b) = abs a + abs b <= 2

apply :: Step -> Button -> Button
apply (Step a b) old@(Button c d) =
  let new = Button (a + c) (b + d) in
    if valid new then new else old

data Path = Path (Button -> Button)

instance Monoid Path where
  mempty = Path id
  mappend (Path one) (Path two) = Path (two . one)

inject :: Char -> Path
inject 'U' = Path $ apply (Step 0 1)
inject 'D' = Path $ apply (Step 0 (-1))
inject 'L' = Path $ apply (Step (-1) 0)
inject 'R' = Path $ apply (Step 1 0)

appPath :: Path -> Button -> Button
appPath (Path f) = f

solve :: String -> String
solve text = fmap name $ tail $ scanl (\b p -> appPath p b) (Button (-2) 0) paths
  where
    ls = lines text
    paths = map (foldMap inject) ls

keypad :: [String]
keypad = [
  "  1  ",
  " 234 ",
  "56789",
  " ABC ",
  "  D  "
  ]

name :: Button -> Char
name (Button x y) = keypad !! (2 - y) !! (2 + x)
