#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.HashTable.IO as H
import           Data.Hashable
import           Data.IORef
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
import           Data.Monoid
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace
import           System.IO.Unsafe
import           System.Mem.StableName
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Util.Util

data Turn = L | S | R
  deriving (Show, Eq, Ord)

data Cart = Cart {
  y :: Int,
  x :: Int,
  direction :: Char,
  turn :: Turn
  }
  deriving (Show, Eq, Ord)

type Grid = Vector (Vector Char)

getCarts :: Text -> [Cart]
getCarts input = [Cart y x c L |
                   (y, line) <- zip [0..] grid,
                   (x, c) <- zip [0..] line,
                   elem c ("><^v" :: String)]
  where
    grid = map (T.unpack) . T.lines $ input

removeCarts :: Text -> Text
removeCarts = T.replace ">" "-" . T.replace "<" "-" . T.replace "^" "|" . T.replace "v" "|"

rotate :: Turn -> Char -> Char
rotate L '<' = 'v'
rotate L '^' = '<'
rotate L '>' = '^'
rotate L 'v' = '>'
rotate R '<' = '^'
rotate R '^' = '>'
rotate R '>' = 'v'
rotate R 'v' = '<'
rotate S d = d

stepTurn :: Turn -> Turn
stepTurn L = S
stepTurn S = R
stepTurn R = L

stepCart :: Grid -> Cart -> Cart
stepCart grid (Cart y x direction turn) = case direction of
  '>' -> mayTurn y (x+1)
  '<' -> mayTurn y (x-1)
  '^' -> mayTurn (y-1) x
  'v' -> mayTurn (y+1) x
  where
    mayTurn y x = case (grid V.! y V.! x) of
      '+' -> Cart y x (rotate turn direction) (stepTurn turn)
      ' ' -> error $ "Off the tracks! at " ++ show x ++ " " ++ show y ++ " : " ++ show (grid V.! y V.! x)
      '/' -> case direction of
               '<' -> Cart y x 'v' turn
               '^' -> Cart y x '>' turn
               '>' -> Cart y x '^' turn
               'v' -> Cart y x '<' turn
      '\\' -> case direction of
               '<' -> Cart y x '^' turn
               '^' -> Cart y x '<' turn
               '>' -> Cart y x 'v' turn
               'v' -> Cart y x '>' turn
      _   -> Cart y x direction turn

stepCarts :: Grid -> [Cart] -> Either (Int,Int) [Cart]
stepCarts grid carts = go (sort carts) (Set.fromList [(x,y) | Cart y x _ _ <- carts])
  where
    go [] _ = Right []
    go (c : cs) occupied = let c' = stepCart grid c in
                             if Set.member (x c', y c') occupied then
                               -- collision
                               Left (x c', y c')
                             else
                               let occupied' = Set.insert (x c', y c') . Set.delete (x c, y c) $ occupied
                               in (c':) <$> go cs occupied'

solve1 :: Grid -> [Cart] -> (Int,Int)
solve1 grid carts = case go carts of
                        Left p -> p
  where
    go :: [Cart] -> Either (Int,Int) [Cart]
    go carts = stepCarts grid carts >>= go

stepCarts2 :: Grid -> [Cart] -> [Cart]
stepCarts2 grid carts = let cmap = Map.fromList [((y,x),c) | c@(Cart y x _ _) <- carts]
                        in go cmap cmap
  where
    go old new
      | Map.null old = Map.elems new
      | otherwise    = let (p, c) = Map.findMin old
                           c' = stepCart grid c
                           p' = (y c', x c')
                       in
                         case Map.lookup p' new of
                           Just _ -> -- collision
                             go (Map.delete p' $ Map.delete p $ old) (Map.delete p' $ Map.delete p $ new)
                           Nothing ->
                             go (Map.delete p $ old) (Map.insert p' c' $ Map.delete p $ new)

solve2 :: Grid -> [Cart] -> (Int,Int)
solve2 grid carts = go carts
  where
    go [Cart y x _ _] = (x,y)
    go carts = go (stepCarts2 grid carts)

main :: IO ()
main = do
  input <- T.readFile "input/13.txt"
  let
    tracks :: Vector (Vector Char)
    tracks = V.fromList . map (V.fromList . T.unpack) . T.lines $ (removeCarts input)
  print (solve1 tracks (getCarts input))
  -- print (getCarts input)
  -- print (stepCarts2 tracks (getCarts input))
  print (solve2 tracks (getCarts input))
