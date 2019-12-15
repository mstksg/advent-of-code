{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Linear.V2
import           Numeric.Search.Integer
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Trifecta as Trifecta

import           Util

data Recipe = Recipe {
  inputs :: [(Integer, Text)],
  outq :: Integer,
  out :: Text
  }
  deriving Show

parse :: Trifecta.Parser a -> Text -> a
parse p txt = case Trifecta.parseString p mempty (T.unpack txt) of
  Trifecta.Success a -> a
  Trifecta.Failure e -> error (show e)

lineP :: (CharParsing m, Monad m) => m Recipe
lineP = do let amt = do n <- read <$> some digit
                        spaces
                        el <- T.pack <$> some upper
                        return (n, el)
           inputs <- (amt `sepBy` text ", ")
           text " => "
           (ct, o) <- amt
           return (Recipe inputs ct o)

parseInput :: Text -> Map Text Recipe
parseInput txt = Map.fromList [(o, r) | r@(Recipe _ _ o) <- rs]
  where
    rs = parse lineP <$> T.lines txt

ceilDiv a b = div (a - 1) b + 1

calcOre :: Map Text Recipe -> Integer -> Text -> Integer
calcOre recipes n fuel = go Map.empty 0 [(n, fuel)]
  where
    go given ore [] = ore
    go given ore ((wanted_ore, "ORE"):ws) =
        go given (ore + wanted_ore) ws
    go given ore ((wanted_el, el):ws) =
      let given_el = Map.findWithDefault 0 el given
          needed = wanted_el - given_el
          recipe = recipes Map.! el
          times = ceilDiv needed (outq recipe)
          new_wanteds = [(times * n, i) | (n, i) <- inputs recipe]
          leftover = times * outq recipe + given_el - wanted_el
      in
        if given_el >= wanted_el then
          go (Map.insert el (given_el - wanted_el) given) ore ws
        else
          -- needed >= 1
          go (Map.insert el leftover given) ore (new_wanteds ++ ws)

solve1 :: Map Text Recipe -> Integer
solve1 recipes = calcOre recipes 1 "FUEL"

solve2 :: Map Text Recipe -> Integer
solve2 recipes = searchFrom (\n -> calcOre recipes n "FUEL" > 1000000000000) 1 - 1
