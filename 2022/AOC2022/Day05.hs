-- |
-- Module      : AOC2022.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day05 (
  day05a,
  day05b,
)
where

import AOC.Common (clearOut, listTup, listV3, readAll)
import AOC.Solver ((:~>) (..))
import Control.DeepSeq (NFData)
import Control.Monad ((<=<))
import Data.Bitraversable (bitraverse)
import Data.Char (isDigit, isLetter)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (transpose)
import Data.List.Split (splitOn)
import GHC.Generics (Generic)
import Linear.V3 (V3 (..))
import Safe.Exact (takeExactMay)

data Instr = Instr {amount :: !Int, source :: !Int, dest :: !Int}
  deriving stock (Eq, Show, Generic)

instance NFData Instr

parseInput ::
  String ->
  Maybe (IntMap [Char], [Instr])
parseInput =
  bitraverse (Just . parseGrid . lines) (parseInstr . lines)
    <=< listTup . splitOn "\n\n"
  where
    parseGrid =
      IM.fromList
        . zip [1 ..]
        . filter (not . null)
        . map (filter isLetter)
        . transpose
    parseInstr =
      traverse $
        fmap (\(V3 amount source dest) -> Instr{..})
          . listV3
          <=< readAll
            . words
            . clearOut (not . isDigit)

runInstr ::
  (Int -> [a] -> [a] -> ([a], [a])) ->
  IntMap [a] ->
  Instr ->
  IntMap [a]
runInstr f stacks (Instr{..}) = IM.fromList [(source, xs'), (dest, ys')] `IM.union` stacks
  where
    xs = stacks IM.! source
    ys = stacks IM.! dest
    (xs', ys') = f amount xs ys

day05 ::
  (Int -> [Char] -> [Char] -> ([Char], [Char])) ->
  (IntMap [Char], [Instr]) :~> String
day05 shifter =
  MkSol
    { sParse = parseInput
    , sShow = id
    , sSolve = \(stacks, instrs) ->
        foldMap (takeExactMay 1) $
          foldl' (runInstr shifter) stacks instrs
    }

day05a :: (IntMap [Char], [Instr]) :~> String
day05a = day05 shiftItems
  where
    shiftItems 0 xs ys = (xs, ys)
    shiftItems _ [] ys = ([], ys)
    shiftItems n (x : xs) ys = shiftItems (n - 1) xs (x : ys)

day05b :: (IntMap [Char], [Instr]) :~> String
day05b = day05 \n xs ys ->
  let (zs, xs') = splitAt n xs
   in (xs', zs ++ ys)
