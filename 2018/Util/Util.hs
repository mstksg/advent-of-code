{-# LANGUAGE FlexibleContexts #-}
module Util.Util where

import           Control.Applicative
import           Control.Monad
import           Data.Char
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
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           System.IO.Unsafe
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

maximumOn f = maximumBy (comparing f)

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () c -> s -> c
parse p s = either (error.show) id (Parsec.parse p "" s)
