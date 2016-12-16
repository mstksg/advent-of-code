import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as I


bytes :: String -> ByteString
bytes = C.pack

md5 :: ByteString -> Digest MD5
md5 = hash

isgood :: Digest MD5 -> Bool
isgood dig = take 5 (show dig) == "00000"

hashes :: String -> [String]
hashes seed = map go [0..]
  where
    ctx :: Context MD5
    ctx = hashUpdate hashInit (bytes seed)
    go n = show $ hashFinalize (hashUpdate ctx (bytes $ show n))

goodhashes :: String -> [String]
goodhashes seed = filter (\digest -> take 5 digest == "00000") (hashes seed)

part1 :: String -> String
part1 seed = take 8 $ map (!!5) (goodhashes seed)

part2 :: String -> [String]
part2 seed = go (goodhashes seed) I.empty
  where
    go (dig : rest) m
      | I.size m == 8 = [answer]
      | otherwise = answer : case pos of
                      Just n -> case I.lookup n m of
                                  Just _ -> go rest m
                                  Nothing -> go rest (I.insert n c m)
                      Nothing -> go rest m
      where
        pc = dig !! 5
        pos = if pc >= '0' && pc <= '7' then
                Just (read [pc])
              else Nothing
        c = dig !! 6
        answer = [case I.lookup n m of
                     Just c -> c
                     Nothing -> '_'
                 | n <- [0..7]]

main :: IO ()
main = putStr $ unlines $ part2 "ffykfhsq"
