#!/usr/bin/runhaskell

import Control.Monad
import Data.List

valid :: [String] -> Bool
valid ws = ws == nub ws

valid2 :: [String] -> Bool
valid2 = valid . map sort

main :: IO ()
main = do
  ps <- map words . lines <$> readFile "input.04"
  print (length $ filter valid ps)
  print (length $ filter valid2 ps)
