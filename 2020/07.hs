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
import qualified Data.Map.Lazy as ML
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
import           Text.Parser.Combinators hiding (count)

import           Scanf
import           Util
import qualified Util.Text as T

input :: Map Text [(Text, Int)]
input = unsafePerformIO (parse p <$> T.readFile "input/07.txt")
  where
    p_color = someText letter ## text " " ## someText letter
    p = fmap Map.fromList . some $ do
      col <- p_color
      text " bags contain "
      let has_children = some $ do
            c_num <- p_nat
            spaces
            c_col <- p_color
            spaces
            if c_num == 1 then text "bag" else text "bags"
            optional (text ", ")
            return (c_col, c_num)
          no_children = text "no other bags" >> return []
      childs <- has_children <|> no_children
      char '.'
      spaces
      return (col, childs)

part1 = Set.size $ follow "shiny gold"
  where
    inverse = Map.fromListWith (++) [(c, [p]) | (p, cs) <- Map.toList input, (c, _) <- cs]
    follow col = case Map.lookup col inverse of
      Just ps -> Set.fromList ps <> foldMap follow ps
      Nothing -> Set.empty

part2 = contained ML.! "shiny gold"
  where
    contained = ML.fromList [(p, cont p cs) | (p, cs) <- Map.toList input]
    cont p cs = sum [n * (1 + (contained ML.! c)) | (c, n) <- cs]
