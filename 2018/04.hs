#!/usr/bin/env stack
-- stack runghc

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Calendar
import           Data.Time.Format
import           System.IO.Unsafe
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

input :: String
input = unsafePerformIO (readFile "input/4.txt")

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () c -> s -> c
parse p s = either (error.show) id (Parsec.parse p "" s)

(##) :: (Applicative f, Monoid a) => f a -> f a -> f a
(##) = liftA2 mappend

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (comparing f)

data Event = Shift Int
           | FallAsleep
           | WakeUp
  deriving (Show, Eq, Ord)

events :: [(String, Event)]
events = sort $ parse (endBy event spaces) input
  where
    event = do
      date <- between (char '[') (char ']') $ some (noneOf "]")
      spaces
      ev <- Shift . read <$> (string "Guard #" *> some digit <* string " begins shift")
            <|> FallAsleep <$ string "falls asleep"
            <|> WakeUp <$ string "wakes up"
      return (date, ev)

labeled :: [(String, Int, Event)]
labeled = foldr go (const []) events undefined
  where
    go (d, Shift n) k _ = k n
    go (d, FallAsleep) k n = (d, n, FallAsleep) : k n
    go (d, WakeUp) k n = (d, n, WakeUp) : k n

byguard :: Map Int [(String, Event)]
byguard = sort <$> Map.fromListWith (++) [(n, [(d, e)]) | (d, n, e) <- labeled]

byminute :: Map Int [(Int, Event)]
byminute = sort . map (\(time, event) -> (read $ last $ splitOn ":" time, event)) <$> byguard

total :: [(Int, Event)] -> Int
total events = foldr go (\(t0, height, total) -> total) events (0, 0, 0)
  where
    go (t, FallAsleep) k (t0, height, total) = k (t, height + 1, (t - t0) * height + total)
    go (t, WakeUp)     k (t0, height, total) = k (t, height - 1, (t - t0) * height + total)

highest :: [(Int, Event)] -> (Int, Int)
highest events = foldr go (\(t0, height, (maxheight, maxt)) -> (maxheight, maxt)) events (0, 0, (0, 0))
  where
    go (t, FallAsleep) k (t0, height, maxheight) = k (t, height + 1, max maxheight (height + 1, t))
    go (t, WakeUp)     k (t0, height, maxheight) = k (t, height - 1, maxheight)

solve1 :: Int
solve1 = gid * gminute
  where
    gid = maximumOn (total . (byminute Map.!)) $ Map.keys byminute
    gminute = snd $ highest (byminute Map.! gid)

solve2 :: Int
solve2 = gid * gminute
  where
    gid = maximumOn (fst . highest . (byminute Map.!)) $ Map.keys byminute
    gminute = snd $ highest (byminute Map.! gid)

main :: IO ()
main = do
  print solve1
  print solve2
