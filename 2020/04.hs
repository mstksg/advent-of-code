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
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count, between)

import           Scanf
import           Util
import qualified Util.Text as T

input :: [Map Text Text]
input = unsafePerformIO (parse p <$> T.readFile "input/04.txt")
  where
    p = some $ do
      fields <- some $ do
        name <- someText letter
        char ':'
        val <- someText (noneOf " \n")
        optional (oneOf " \n")
        return (name, val)
      spaces
      return (Map.fromList fields)

part1 = sum [1 | pass <- input, val pass]
  where
    required = T.words "byr iyr eyr hgt hcl ecl pid"
    val mp = all (\f -> Map.member f mp) required

between a b x = guard (a <= x && x <= b)

part2 = sum [1 | pass <- input, valid pass]
  where
    required = T.words "byr iyr eyr hgt hcl ecl pid"
    valid mp = all (\f -> Map.member f mp) required && all (uncurry val) (Map.toList mp)
    val "byr" x = pval (p_nat >>= between 1920 2002) x
    val "iyr" x = pval (p_nat >>= between 2010 2020) x
    val "eyr" x = pval (p_nat >>= between 2020 2030) x
    val "hgt" x = pval (do num <- p_nat
                           asum [do text "cm"
                                    guard (150 <= num && num <= 193),
                                 do text "in"
                                    guard (59 <= num && num <= 76)]) x
    val "hcl" x = pval (char '#' >> replicateM 6 hexDigit) x
    val "ecl" x = elem x (T.words "amb blu brn gry grn hzl oth")
    val "pid" x = pval (replicateM 9 digit) x
    val "cid" x = True
