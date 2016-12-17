import System.IO
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

data P = P Integer Integer deriving (Show, Eq, Ord)

mul :: P -> P -> P
mul (P a b) (P c d) = P (a*c - b*d) (a*d + b*c)

add :: P -> P -> P
add (P a b) (P c d) = P (a + c) (b + d)

instance Num P where
  fromInteger n = P n 0
  (+) = add
  (*) = mul
  negate (P a b) = P (-a) (-b)
  (-) a b = a + (-b)
  signum = undefined
  abs = undefined

rotate :: Char -> P -> P
rotate '0' p = p
rotate 'L' p = mul p (P 0 1)
rotate 'R' p = mul p (P 0 (-1))

type Direction = P

data Path = Path Direction P deriving (Show, Eq)

instance Monoid Path where
  mempty = Path 1 0
  mappend (Path d1 e1) (Path d2 e2) = Path (d2 * d1) (e1 + d1 * e2)

inject :: (Char, Integer) -> Path
inject (t,n) = Path (rotate t 1) (rotate t (P n 0))

size :: P -> Integer
size (P a b) = abs a + abs b

total :: Path -> Integer
total (Path d p) = size p

-- solve :: String -> String
-- solve text = show $ foldr (\(t, d) p -> add (P 0 d) (rotate t p)) (P 0 0 ) ds
--   where
--     w = splitOn ", " text
--     ds = [(turn, read distance) | (turn:distance) <- w]

solve :: String -> String
solve text = show $ fmap size $ go ps Set.empty
  where
    w = splitOn ", " text
    ds = [(turn, read distance) | (turn:distance) <- w]
    ds' = foldMap (\(turn, distance) -> (turn, 1) : replicate (distance - 1) ('0', 1)) ds
    ps = [p | Path d p <- scanl mappend mempty $ map inject ds']

    go [] _ = Nothing
    go (p:ps) set = if Set.member p set then
                      Just p
                    else go ps (Set.insert p set)

main :: IO ()
main = do text <- getContents
          putStrLn (solve text)
