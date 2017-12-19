#!/usr/bin/runhaskell
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
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

garbage :: CharParsing m => m String
garbage = char '<' *> (fold <$> many garbagePiece) <* char '>'
  where
    garbagePiece = (pure <$> satisfy (\c -> c /= '>' && c /= '!'))
                   <|> (char '!' *> anyChar *> pure [])

type Garbage = String
newtype Group = Group [Either String Group]
  deriving (Show, Eq, Ord)

pgroup :: CharParsing m => m Group
pgroup = Group <$> (char '{' *> sepBy (fmap Left garbage <|> fmap Right pgroup) (char ',') <* char '}')

score :: Group -> Int
score g = go 1 g
  where
    go n (Group xs) = n + sum [go (n + 1) g | Right g <- xs]

removed :: Group -> Int
removed (Group xs) = sum (map go xs)
  where
    go (Left gs) = length gs
    go (Right g) = removed g

main :: IO ()
main = do
  text <- readFile "input/09"
  let Right g = Parsec.parse pgroup "input/09" text
  print $ score g
  print $ removed g
