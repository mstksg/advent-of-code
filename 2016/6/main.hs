import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Tuple
import Data.List
import System.IO

decode :: [Char] -> Char
decode cs = snd $ last $ sort $ map swap $ MS.toOccurList ms
  where
    ms = MS.fromList cs

decode2 :: [Char] -> Char
decode2 cs = snd $ head $ sort $ map swap $ MS.toOccurList ms
  where
    ms = MS.fromList cs

text :: IO String
text = readFile "input.txt"

part1 text = map decode $ transpose $ lines text

part2 text = map decode2 $ transpose $ lines text
