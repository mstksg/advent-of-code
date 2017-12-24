#!/usr/bin/runhaskell
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List
import           Data.List.Split
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Semigroup
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Numeric
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

cspan :: Functor f => Int -> Int -> LensLike' f [a] [a]
cspan off len f xs
  | off + len < n = (\ys -> take off xs ++ ys ++ drop (off + len) xs) <$> f (take len $ drop off $ xs)
  | otherwise     = (\ys -> drop (n - off) ys ++
                            (take (n - len) $ drop (off + len - n) $ xs) ++
                            take (n - off) ys)
                    <$> f (drop off xs ++ take (off + len - n) xs)
  where
    n = length xs

data HashState = HashState Int Int [Word8]

runRound :: Int -> HashState -> HashState
runRound len (HashState cp skip xs) = HashState cp' skip' xs'
  where
    cp' = (cp + len + skip) `mod` (length xs)
    skip' = skip + 1
    xs' = over (cspan cp len) reverse xs

digest :: HashState -> [Word8]
digest (HashState _ _ xs) = map (foldr xor 0) $ chunksOf 16 xs

showDigest :: [Word8] -> String
showDigest = foldMap ((\s -> replicate (2-length s) '0' ++ s) . flip showHex "")

hashData :: String -> [Word8]
hashData = digest . foldr runRound (HashState 0 0 [0..255]) . reverse . fold . replicate 64 . (++ [ 17, 31, 73, 47, 23]) . map ord
