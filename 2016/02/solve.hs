import Data.Monoid

data Step = Step Int Int deriving (Show)
data Button = Button Int Int deriving (Show)

clamp :: Button -> Button
clamp (Button a b) = Button (cl a) (cl b)
  where
    cl x = max (-1) (min x 1)

apply :: Step -> Button -> Button
apply (Step a b) (Button c d) = clamp (Button (a + c) (b + d))

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
solve text = show $ fmap name $ scanl (\b p -> appPath p b) (Button 0 0) paths
  where
    ls = lines text
    paths = map (foldMap inject) ls

name :: Button -> Integer
name (Button (-1) 1) = 1
name (Button 0 1) = 2
name (Button 1 1) = 3
name (Button (-1) 0) = 4
name (Button 0 0) = 5
name (Button 1 0) = 6
name (Button (-1) (-1)) = 7
name (Button 0 (-1)) = 8
name (Button 1 (-1)) = 9
