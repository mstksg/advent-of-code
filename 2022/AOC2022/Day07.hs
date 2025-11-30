-- |
-- Module      : AOC2022.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day07 (
  day07a,
  day07b,
)
where

import AOC.Solver ((:~>) (..))
import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.List (find, foldl', sort, tails)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Tuple.Strict (T2 (..), ssnd)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data CDDest = Root | GoUp | GoDown String
  deriving stock (Eq, Show, Ord, Generic)

instance NFData CDDest

data Instr
  = CD CDDest
  | File Int
  deriving stock (Eq, Show, Ord, Generic)

instance NFData Instr

parseInstr :: String -> Maybe Instr
parseInstr i = case words i of
  ["$", "cd", "/"] -> Just $ CD Root
  ["$", "cd", ".."] -> Just $ CD GoUp
  ["$", "cd", d] -> Just $ CD (GoDown d)
  [n, _] | Just sz <- readMaybe n -> Just $ File sz
  _ -> Nothing

buildSizes :: [Instr] -> Map [String] Int
buildSizes = ssnd . foldl' go (T2 [] M.empty)
  where
    go (T2 currDir mp) = \case
      CD Root -> T2 [] mp
      CD GoUp -> T2 (drop 1 currDir) mp
      CD (GoDown d) -> T2 (d : currDir) mp
      File sz ->
        T2 currDir $
          M.unionWith (+) mp $
            M.fromList (map (,sz) (tails currDir))

day07a :: [Instr] :~> Int
day07a =
  MkSol
    { sParse = Just . mapMaybe parseInstr . lines
    , sShow = show
    , sSolve = Just . sum . M.filter (<= 100000) . buildSizes
    }

day07b :: [Instr] :~> Int
day07b =
  MkSol
    { sParse = Just . mapMaybe parseInstr . lines
    , sShow = show
    , sSolve = \xs ->
        let sizes = buildSizes xs
            totalSize = sizes M.! []
         in find (\s -> (totalSize - s) <= 70000000) $ sort (toList sizes)
    }
