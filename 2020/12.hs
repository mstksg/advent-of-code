{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Function.Memoize
import           Data.Int
import           Data.List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Debug.Trace
import           Linear.Matrix
import           Linear.V2
import           Linear.V4
import           Linear.Vector
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import qualified Util.Text as T

input :: [(Char, Int)]
input = unsafePerformIO (parse p <$> T.readFile "input/12.txt")
  where
    p = some $ do
      (,) <$> letter <*> p_nat <* spaces

rotd :: Int -> V2 Int -> V2 Int
rotd d (V2 x y) = V2 (x * cosd - y * sind) (x * sind + y * cosd)
  where
    cosd = round (cos (fromIntegral d * pi / 180))
    sind = round (sin (fromIntegral d * pi / 180))

rotm :: Int -> M22 Int
rotm d = V2 (V2 cosd (-sind)) (V2 sind cosd)
  where
    cosd = round (cos (fromIntegral d * pi / 180))
    sind = round (sin (fromIntegral d * pi / 180))

manhatten :: V2 Int -> Int
manhatten v = sum (abs <$> v)

data V5 a = V5 a a a a a
  deriving (Functor, Foldable, Traversable, Show)

instance Applicative V5 where
  pure a = V5 a a a a a
  V5 f g h i j <*> V5 a b c d e = V5 (f a) (g b) (h c) (i d) (j e)

instance Num a => Num (V5 a) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs = error "abs"
  signum = error "signum"

type Step = V5 (V5 Int)

instance Semigroup (V5 (V5 Int)) where
  (<>) = flip (!*!)
instance Monoid (V5 (V5 Int)) where
  mempty = identity

instance Additive V5 where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

part1 = foldMap toStep input
  where
    -- runStep (Step v _ f) = v + (f !* V2 1 0)
    runStep m55 = case m55 !* V5 1 0 0 1 0 of V5 1 x y _ _ -> V2 x y
    toStep ('N', d) = (V5
                       (V5 1 0 0 0 0) -- 1
                       (V5 0 1 0 0 0) -- x
                       (V5 d 0 1 0 0) -- y
                       (V5 0 0 0 1 0) -- dx
                       (V5 0 0 0 0 1) -- dy
                      )
    toStep ('E', d) = (V5
                       (V5 1 0 0 0 0) -- 1
                       (V5 d 1 0 0 0) -- x
                       (V5 0 0 1 0 0) -- y
                       (V5 0 0 0 1 0) -- dx
                       (V5 0 0 0 0 1) -- dy
                      )
    toStep ('S', d) = (V5
                       (V5 1 0 0 0 0) -- 1
                       (V5 0 1 0 0 0) -- x
                       (V5 (-d) 0 1 0 0) -- y
                       (V5 0 0 0 1 0) -- dx
                       (V5 0 0 0 0 1) -- dy
                      )
    toStep ('W', d) = (V5
                       (V5 1 0 0 0 0) -- 1
                       (V5 (-d) 1 0 0 0) -- x
                       (V5 0 0 1 0 0) -- y
                       (V5 0 0 0 1 0) -- dx
                       (V5 0 0 0 0 1) -- dy
                      )
    toStep ('F', d) = (V5
                       (V5 1 0 0 0 0) -- 1
                       (V5 0 1 0 d 0) -- x
                       (V5 0 0 1 0 d) -- y
                       (V5 0 0 0 1 0) -- dx
                       (V5 0 0 0 0 1) -- dy
                      )
    toStep ('L', d) = case rotm d of
                        V2 (V2 a b) (V2 c d) ->
                          (V5
                           (V5 1 0 0 0 0) -- 1
                           (V5 0 1 0 0 0) -- x
                           (V5 0 0 1 0 0) -- y
                           (V5 0 0 0 a b) -- dx
                           (V5 0 0 0 c d) -- dy
                          )
    toStep ('R', d) = toStep ('L', -d)

part2 = manhatten . fst $ foldl go (V2 0 0, V2 10 1) input
  where
    go (pos,wayp) ('N', d) = (pos, wayp + V2 0 d)
    go (pos,wayp) ('E', d) = (pos, wayp + V2 d 0)
    go (pos,wayp) ('S', d) = (pos, wayp + V2 0 (-d))
    go (pos,wayp) ('W', d) = (pos, wayp + V2 (-d) 0)
    go (pos,wayp) ('F', d) = (pos + fmap (d*) wayp, wayp)
    go (pos,wayp) ('L', d) = (pos, rotd d wayp)
    go (pos,wayp) ('R', d) = (pos, rotd (-d) wayp)
