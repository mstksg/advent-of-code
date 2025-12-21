-- |
-- Module      : AOC2025.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day12 (
  day12a,
)
where

import AOC.Common (countTrue, listV2)
import AOC.Common.Point (Point, parseAsciiSet)
import AOC.Solver (noFail, (:~>) (..))
import Control.Monad ((<=<))
import Data.Bitraversable (bitraverse)
import Data.List (uncons)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S
import Safe (initMay, lastMay)
import Text.Read (readMaybe)

pVec :: String -> Maybe Point
pVec = listV2 <=< traverse readMaybe . splitOn "x" <=< initMay

day12a :: ([Set Point], [(Point, [Int])]) :~> Int
day12a =
  MkSol
    { sParse = \xs -> do
        let chunkies = splitOn [""] $ lines xs
        blockies <- map (parseAsciiSet (== '#') . unlines . drop 1) <$> initMay chunkies
        thingies <- traverse (bitraverse pVec (traverse readMaybe) <=< uncons . words) =<< lastMay chunkies
        pure (blockies, thingies)
    , sShow = show
    , sSolve = noFail \(blocks, areas) -> countTrue (go (map S.size blocks)) areas
    }
  where
    go blockSizes (bounds, ns) = product bounds >= sum (zipWith (*) blockSizes ns)
