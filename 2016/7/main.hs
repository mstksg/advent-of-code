import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

part1 :: String -> Int
part1 text = length . filter part1TLS . lines $ text

part1TLS :: String -> Bool
part1TLS str = any containsABBA out && not (any containsABBA inn)
  where
    (out, inn) = splitSeq str

isABBA :: String -> Bool
isABBA [a, b, c, d] = a == d && b == c && a /= b
isABBA xs = error ("not 4 letters: " ++ show xs)

containsABBA :: String -> Bool
containsABBA xs
  | length xs < 4      = False
  | isABBA (take 4 xs) = True
  | otherwise          = containsABBA (drop 1 xs)

splitSeq :: String -> ([String], [String])
splitSeq str = go parts
  where
    parts = splitOneOf "[]" str
    go [] = ([], [])
    go [out] = ([out], [])
    go (out:inn:rest) = let (o, i) = go rest in
                          (out : o, inn : i)

part2 :: String -> Int
part2 text = length . filter part2SSL . lines $ text

part2SSL :: String -> Bool
part2SSL str = any (\aba -> Set.member (invertABA aba) babs) abas
  where
    (out, inn) = splitSeq str
    abas = foldMap getABAs out
    babs = Set.fromList (foldMap getABAs inn)

getABAs :: String -> [String]
getABAs (a:b:c:xs)
  | a == c && a /= b = [a,b,c] : getABAs (b:c:xs)
  | otherwise        = getABAs (b:c:xs)
getABAs _ = []

invertABA :: String -> String
invertABA [a,b,_] = [b, a, b]

text :: IO String
text = readFile "input.txt"
