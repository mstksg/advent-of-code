{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.FiniteField.PrimeField
import           Data.Foldable
import qualified Data.Graph.Inductive.Graph as FGL
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.MST as FGL
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Debug.Trace
import           Linear.Matrix
import           Linear.V2
import           Linear.V3
import           System.IO.Unsafe

import           AStar
import           Util

import           Text.Parser.Char
import           Text.Parser.Combinators

data Step = Rev | Deal Integer | Cut Integer
  deriving (Show, Eq, Ord)

pLine :: (Parser p) => p Step
pLine = asum [
  do text "deal with increment "
     n <- read <$> some digit
     return (Deal n),
  do text "deal into new stack"
     return Rev,
  do text "cut "
     n <- read <$> ((string "-" <|> pure "") ## some digit)
     return (Cut n)
  ]

-- runStep :: Integer -> Step -> (Integer -> Integer) -> (Integer -> Integer)
-- runStep p Rev f x = (p - 1) - f x
-- runStep p (Cut n) f x = mod (f x - n) p
-- runStep p (Deal n) f x = mod (f x * n) p

-- runStep' :: Integer -> Step -> (Integer, Integer) -> (Integer, Integer)
-- runStep' p Rev (a, b) = (-a, -b + (p - 1))
-- runStep' p (Cut n) (a, b) = (a, b - n)
-- runStep' p (Deal n) (a, b) = (a*n, b*n)

-- eval p (a, b) x = mod (a * x + b) p

(.*) a x = fmap (*a) x

newtype M a = M { getM :: M33 a }
  deriving Functor

instance Num a => Num (M a) where
  fromInteger n = M (identity !!* fromInteger n)
  (*) (M a) (M b) = M (a !*! b)
  (+) (M a) (M b) = M (a !+! b)

runStepM :: Integer -> Step -> M Integer
runStepM p step = M $ case step of
  Rev -> V3 (-a) (-b + (p - 1) .* c) c
  Cut n -> V3 a (b - n .* c) c
  Deal n -> V3 (n .* a) (n .* b) c
  where
    a = V3 1 0 0
    b = V3 0 1 0
    c = V3 0 0 1

evalM m x = case (getM m !* V3 1 0 1) of
  V3 a b c -> (a * x + b)

input :: [Step]
input = unsafePerformIO $ map (parse pLine) . T.lines <$> T.readFile "input/22.txt"

solve1 = evalM m 2019
  where
    m = product $ map (fmap fromIntegral . runStepM 10007) (reverse input) :: M (PrimeField 10007)

solve2 = (2020 - b) / a
  where
    m = product $ map (fmap fromIntegral . runStepM 119315717514047) (reverse input) :: M (PrimeField 119315717514047)
    m' = m ^ 101741582076661
    V3 a b _ = getM m' !* V3 1 0 1

main :: IO ()
main = do
  print ()
