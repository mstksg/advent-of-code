-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
module AOC.Challenge.Day04
  ( day04a,
    day04b,
  )
where

import AOC.Common (listTup)
import AOC.Solver (noFail, (:~>) (..))
import Control.Monad ((<=<))
import Control.Monad.State (evalState, state)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import Safe (tailMay)
import Text.Read (readMaybe)

parseCard :: String -> Maybe (Set Int, Set Int)
parseCard =
  listTup
    <=< traverse (fmap S.fromList . traverse readMaybe . words) . splitOn "|"
    <=< tailMay . dropWhile (/= ':')

cardWins :: (Set Int, Set Int) -> Int
cardWins = S.size . uncurry S.intersection

day04a :: [(Set Int, Set Int)] :~> Int
day04a =
  MkSol
    { sParse = traverse parseCard . lines,
      sShow = show,
      sSolve =
        noFail $
          sum . map ((2 ^) . subtract 1) . filter (> 0) . map cardWins
    }

day04b :: [(Set Int, Set Int)] :~> Int
day04b =
  MkSol
    { sParse = traverse parseCard . lines,
      sShow = show,
      sSolve = noFail \cards ->
        let ixedCards = M.fromList $ zip [1 ..] cards
         in sum . flip evalState (1 <$ ixedCards) $
              for (M.toList ixedCards) \(i, (a, b)) -> state \currState ->
                let amount = currState M.! i
                    rest = M.delete i currState
                    n = S.size $ a `S.intersection` b
                    newCards = M.fromList ((,amount) . (+ i) <$> [1 .. n])
                 in (currState M.! i, M.unionWith (+) newCards rest)
    }
