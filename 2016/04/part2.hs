import Data.Foldable
import Data.Monoid
import Data.List
import Data.List.Split
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Ord
import Data.Tuple
import Data.Char

data Room = Room {
  name :: String,
  sector :: Integer,
  checksum :: String
  }
  deriving Show

isReal :: Room -> Bool
isReal (Room name sector checksum) = cksum name == checksum

cksum :: String -> String
cksum name = take 5 (map fst occur)
  where
    ms = MS.fromList (filter isAlpha name)
    occur = sortOn (\(c, n) -> (Down n, c))  (MS.toOccurList ms)

solve :: [Room] -> String
solve rooms = show $ foldMap go rooms
  where
    -- go r = if isReal r then
    --          Sum (sector r) else mempty
    go r = if decrypt r == "northpole-object-storage" then
             [r]
             else []

rotate :: Integer -> Char -> Char
rotate n c
  | isAlpha c = chr (ord 'a' + mod (ord c - ord 'a' + fromIntegral n) 26)
  | otherwise = c

decrypt :: Room -> String
decrypt r = map (rotate (sector r)) (name r)

decode :: String -> Room
decode line = Room name sector check
  where
    [first, check'] = splitOn "[" line
    parts = splitOn "-" first
    name = intercalate "-" (init parts)
    sector = read (last parts)
    check = init check'

readInput :: String -> [Room]
readInput text = map decode (lines text)

main :: IO ()
main = do
  text <- readFile "input.txt"
  putStrLn $ solve (readInput text)
