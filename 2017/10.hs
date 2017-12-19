#!/usr/bin/runhaskell
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Functor
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List
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
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

data Step = Step Int Int
  deriving (Show, Eq, Ord)

instance Monoid Step where
  mempty = Step 0 0
  mappend (Step x y) (Step w z) = Step (x + w) (y + z)


parseStep :: CharParsing m => m Step
parseStep = choice [try $ Step 1 1 <$ string "ne",
                    try $ Step 1 (-1) <$ string "se",
                    try $ Step (-1) 1 <$ string "nw",
                    try $ Step (-1) (-1) <$ string "sw",
                    try $ Step 0 (-2) <$ string "s",
                    try $ Step 0 2 <$ string "n"]

parsePath :: CharParsing m => m [Step]
parsePath = sepBy parseStep (char ',')

distance :: Step -> Int
distance (Step x y)
  | a == 0    = b `div` 2
  | b == 0    = a
  | otherwise = 1 + distance (Step (a-1) (b-1))
  where
    a = abs x
    b = abs y

main :: IO ()
main = do
  text <- readFile "input/10"
  -- let text = "se,sw,se,sw,sw"
  let Right g = Parsec.parse parsePath "input/10" text
  print $ distance (fold g)
  print $ maximum $ map distance $ scanl mappend mempty g
