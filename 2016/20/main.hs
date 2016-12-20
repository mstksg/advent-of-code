import Data.Foldable
import Data.List
import Data.List.Split

data Jump a = L a | R a deriving (Show, Eq)

cmap :: Jump a -> (a, Char)
cmap (L a) = (a, '(')
cmap (R a) = (a, ')')

instance Ord a => Ord (Jump a) where
  x <= y = cmap x <= cmap y

data SL a = Zero a | Up Int deriving (Show, Eq, Ord)

invert :: Int -> [(Int, Int)] -> [(Int, Int)]
invert limit blocks = go (Zero 0) (jump ++ [L limit])
  where
    jump = sort $ fold [[L a, R b] | (a,b) <- blocks]
    go _ [] = []
    go (Zero c) (L x : xs)
      | x > c     = (c, x-1) : go (Up 1) xs
      | otherwise =            go (Up 1) xs
    go (Up n) (L _ : xs) = go (Up (n+1)) xs
    go (Zero c) (R _ : xs) = error "underflow"
    go (Up 1) (R x : xs) = go (Zero (x+1)) xs
    go (Up n) (R _ : xs) = go (Up (n-1)) xs

solve :: [(Int, Int)] -> String
solve blocks = show $ (jump, go 0 0 jump)
  where
    jump = sort $ fold [[L a, R b] | (a,b) <- blocks]
    go c 0 [] = c
    go c 0 ((L x) : xs)
      | x > c     =  c
      | otherwise = go c 1 xs
    go c 1 ((R x) : xs) = go (x+1) 0 xs
    go c n ((L x) : xs) = go c (n+1) xs
    go c n ((R x) : xs) = go c (n-1) xs

decode :: String -> [(Int, Int)]
decode text = [(read l, read r) | [l, r] <- ls]
  where
    ls = map (splitOn "-") (lines text)
