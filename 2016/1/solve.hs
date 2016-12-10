import System.IO
import Data.List.Split

data P = P Integer Integer deriving (Show, Eq)

mul :: P -> P -> P
mul (P a b) (P c d) = P (a*c - b*d) (a*d + b*c)

add :: P -> P -> P
add (P a b) (P c d) = P (a + c) (b + d)

rotate :: Char -> P -> P
rotate 'L' p = mul p (P 0 1)
rotate 'R' p = mul p (P 0 (-1))

solve :: String -> String
solve text = show $ foldr (\(t, d) p -> add (P 0 d) (rotate t p)) (P 0 0 ) ds
  where
    w = splitOn ", " text
    ds = [(turn, read distance) | (turn:distance) <- w]

main :: IO ()
main = do text <- getContents
          putStrLn (solve text)
