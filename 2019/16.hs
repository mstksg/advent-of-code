{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Foldable
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as U
import           Debug.Trace
import           Linear.V2

import           Util

sums :: U.Vector Int -> U.Vector Int
sums xs = U.cons 0 (U.scanl1 (+) xs)

sumOf :: Int -> Int -> U.Vector Int -> Int
sumOf a b ss = ss U.! (min b (U.length ss - 1)) - ss U.! (min a (U.length ss - 1))

fft :: U.Vector Int -> U.Vector Int
fft xs = trace (show (U.take 8 xs)) $ U.generate (U.length xs) go
  where
    ss = sums xs
    -- go i = let mask = drop 1 $ cycle $ foldMap (replicate (i+1)) [0, 1, 0, -1]
    --        in abs (sum (zipWith (*) (U.toList xs) mask)) `mod` 10
    go i = let c = i + 1
               total = sum [ sumOf (4*c*j + 1 * c - 1) (4*c*j + 2 * c - 1) ss
                           | j <- [0..(U.length ss `div` (4*c))+1]]
                       - sum [ sumOf (4*c*j + 3 * c - 1) (4*c*j + 4 * c - 1) ss
                             | j <- [0..(U.length ss `div` (4*c))+1]]
           in abs total `mod` 10

input :: Integer
input = 59775675999083203307460316227239534744196788252810996056267313158415747954523514450220630777434694464147859581700598049220155996171361500188470573584309935232530483361639265796594588423475377664322506657596419440442622029687655170723364080344399753761821561397734310612361082481766777063437812858875338922334089288117184890884363091417446200960308625363997089394409607215164553325263177638484872071167142885096660905078567883997320316971939560903959842723210017598426984179521683810628956529638813221927079630736290924180307474765551066444888559156901159193212333302170502387548724998221103376187508278234838899434485116047387731626309521488967864391


solve1 :: Integer -> String
solve1 n = foldMap show $ U.toList $ U.take 8 (iterate fft dat !! 100)
  where
    dat = U.fromList $ map (read.pure) $ show n :: U.Vector Int

solve2 :: Integer -> String
solve2 n = foldMap show $ U.toList $ U.take 8 $ U.drop offset $ (iterate fft dat !! 100)
  where
    real = fold $ replicate 10000 (show n)
    dat = U.fromList $ map (read.pure) $ real
    offset = read (take 7 real) :: Int

main :: IO ()
main = do
  putStrLn (solve1 input)
  putStrLn (solve2 input)
