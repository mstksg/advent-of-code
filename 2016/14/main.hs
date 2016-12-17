import Crypto.Hash
import Data.List
import qualified Data.ByteString.Char8 as C
import Control.Parallel.Strategies

triplet :: String -> Maybe Char
triplet (a:b:c:xs)
  | a == b && b == c = Just a
  | otherwise = triplet (b:c:xs)
triplet _ = Nothing

stretched :: Digest MD5 -> String
stretched ctx = iterate go (show ctx) !! 2016
  where
    go str = show (hash (C.pack str) :: Digest MD5)

hashes :: String -> [String]
hashes seed = map go [0..]
  where
    go n = stretched (hashFinalize $ hashUpdate ctx (C.pack (show n)))
    ctx :: Context MD5
    ctx = hashUpdate hashInit (C.pack seed)

keys :: [String] -> [Bool]
keys (h:hs) = maybekey : keys hs
  where
    maybekey = case triplet h of
      Just c -> any (replicate 5 c `isInfixOf`) (take 1000 hs)
      Nothing -> False

part1 :: [Bool] -> Int
part1 ks = go 0 0 ks
  where
    go i 64 _ = i - 1
    go i n (True : ks) = go (i+1) (n+1) ks
    go i n (False : ks) = go (i+1) n ks

main :: IO ()
main = let h = (hashes "jlmsuwbz")
       in print (part1 . keys $ using h (parBuffer 1000 rseq))
