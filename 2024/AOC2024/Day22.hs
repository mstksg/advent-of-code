-- |
-- Module      : AOC2024.Day22
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day22 (
  day22a,
  day22b,
)
where

import AOC.Common (strictIterate, (!!!))
import AOC.Common.Parser (pDecimal, parseMaybe', sepByLines)
import AOC.Solver (noFail, type (:~>) (..))
import Control.Monad (unless)
import Data.Bits (Bits (shift, xor, (.&.)))
import Data.Foldable (for_)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS
import Safe.Foldable (maximumMay)

step :: Int -> Int
step = prune . phase3 . prune . phase2 . prune . phase1
  where
    phase1 n = (n `shift` 6) `xor` n
    phase2 n = (n `shift` (-5)) `xor` n
    phase3 n = (n `shift` 11) `xor` n
    prune = (.&. 16777215)

day22a :: [Int] :~> Int
day22a =
  MkSol
    { sParse = parseMaybe' $ sepByLines pDecimal
    , sShow = show
    , sSolve = noFail $ sum . map ((!!! 2000) . strictIterate step)
    }

day22b :: [Int] :~> Int
day22b =
  MkSol
    { sParse = sParse day22a
    , sShow = show
    , sSolve = \xs -> do
        let serieses = take 2000 . map (`mod` 10) . strictIterate step <$> xs
            tots = VS.create do
              v <- MVS.replicate maxSeq 0
              for_ serieses \series -> do
                seens <- MVS.replicate maxSeq False
                for_ (chompChomp series) \(i, n) -> do
                  seen <- MVS.exchange seens i True
                  unless seen $
                    MVS.modify v (+ n) i
              pure v
        maximumMay $ VS.toList tots
    }
  where
    maxSeq = encodeSeq [9, 9, 9, 9]

encodeSeq :: [Int] -> Int
encodeSeq = sum . zipWith (\i x -> x * 19 ^ (i :: Int)) [0 ..] . map (+ 9)

chompChomp :: [Int] -> [(Int, Int)]
chompChomp (a : b : c : d : e : fs) =
  (encodeSeq [da, db, dc, dd], e) : chompChomp (b : c : d : e : fs)
  where
    da = b - a
    db = c - b
    dc = d - c
    dd = e - d
chompChomp _ = []
