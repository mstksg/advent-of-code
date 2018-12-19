#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import           Data.IORef
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
import           Data.Monoid
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace
import           System.IO.Unsafe
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

import           Util.Util

input :: String
input = unsafePerformIO (readFile "input/12.txt")

rules :: Map String String
rules = Map.fromList [(i,o) | [i, o] <- map (splitOn " => ") (lines input)]

initial :: Set Int
initial = Set.fromList [i | (i, c) <- zip [0..] start, c == '#']
  where
    start = splitOn ": " (head (lines input)) !! 1

extend :: Set Int -> Map Int String
extend plants
  | Set.null plants = Map.empty
  | otherwise = Map.fromList [(i, go i) | i <- [Set.findMin plants - 2 .. Set.findMax plants + 2]]
  where
    go i = [if Set.member j plants then '#' else '.' | j <- [i-2 .. i+2]]

step :: Set Int -> Set Int
step plants = Map.keysSet $ Map.filter (\v -> rules Map.! v == "#") $ extend plants

solve1 :: Int
solve1 = sum $ iterate step initial !! 20

solve2 :: Int
solve2 = sum $ iterate step initial !! 50000

data Cell = Small Int String | Large Int Cell Cell Cell
  deriving (Show, Eq)



hash :: Cell -> Int
hash (Small n _) = n
hash (Large n _ _ _) = n

cache :: IORef (Int, Map (Either String (Int,Int)) Cell)
cache = unsafePerformIO (newIORef (1, Map.empty))

lookupCell :: Either String (Int,Int) -> Cell -> IO Cell
lookupCell key (Small _ str) = do deepseq key (return ())
                                  (next, cells) <- readIORef cache
                                  case Map.lookup key cells of
                                    Just c -> return c
                                    Nothing -> do
                                      let cell = Small next str
                                          cells' = Map.insert key cell cells
                                      writeIORef cache (next + 1, cells')
                                      return cell
lookupCell key (Large _ l r x) = do deepseq key (return ())
                                    (next, cells) <- readIORef cache
                                    case Map.lookup key cells of
                                      Just c -> return c
                                      Nothing -> do
                                        let cell = Large next l r x
                                            cells' = Map.insert key cell cells
                                        writeIORef cache (next + 1, cells')
                                        return cell

findSmall :: String -> Cell
findSmall xs = unsafePerformIO $ lookupCell (Left xs) (Small undefined xs)

findCell :: Cell -> Cell -> Cell
findCell (Small _ xs) (Small _ ys)
  | length (xs ++ ys) < 8  =  findSmall (xs ++ ys)
findCell cell1 cell2 = unsafePerformIO $ lookupCell (Right (hash cell1, hash cell2)) (Large undefined cell1 cell2 (run cell1 cell2))

result :: Cell -> Cell
result (Small _ xs)
  | length xs /= 8 = error "not 8"
result (Small _ xs) = let set = step $ Set.fromList [i | (i, c) <- zip [0..] xs, c == '#']
                          str' = [if Set.member i set then '#' else '.'
                                 | i <- [2..5]]
                      in findSmall str'
result (Large _ _ _ r) = r

slice :: Cell -> (Cell, Cell)
slice (Large _ x y _) = (x, y)
slice (Small _ xs) = (findSmall $ take (length xs `div` 2) xs,
                      findSmall $ drop (length xs `div` 2) xs)

run :: Cell -> Cell -> Cell
run (Small _ xs) (Small _ ys)
  | length (xs ++ ys) == 8 = let str = xs ++ ys
                                 set = step $ Set.fromList [i | (i, c) <- zip [0..] str, c == '#']
                                 str' = [if Set.member i set then '#' else '.'
                                        | i <- [2..5]]
                             in findSmall str'
  | otherwise = undefined
run (Large _ a b x1) (Large _ c d x3) = let x2 = result (findCell b c)
                                            y1 = findCell x1 x2
                                            y2 = findCell x2 x3
                                        in findCell (result y1) (result y2)

display :: Cell -> String
display (Small _ xs) = xs
display (Large _ a b _) = display a <> display b

convert :: String -> Cell
convert xs
  | 2 ^ round (logBase 2 (fromIntegral n)) /= n = error "not power of 2"
  | n >= 8    = findCell (convert x1) (convert x2)
  | otherwise = findSmall xs
  where
    n = length xs
    m = n `div` 2
    x1 = (take m xs)
    x2 = (drop m xs)

-- stepN :: Int -> Cell -> Cell
-- stepN

main :: IO ()
main = do print solve1
          let start = splitOn ": " (head (lines input)) !! 1
              padded = start ++ replicate (128 - length start) '.'
          print padded
          print (display $ result $ convert $ padded)
          -- Set.fromList [findSmall [c] | c <- start]
          -- let x = findCell zero one
          --     x2 = findCell x x
          -- putStrLn (display x2)
          -- putStrLn ("    " ++ display (result x2))
          -- putStrLn ("      " ++ display (result (result x2)))
