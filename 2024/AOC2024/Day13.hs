-- |
-- Module      : AOC2024.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day13 (
  day13a,
  day13b,
)
where

import AOC.Common (inv22Int)
import AOC.Common.Parser (pDecimal, parseMaybe', sequenceSepBy)
import AOC.Common.Point (Point, V2 (..))
import AOC.Solver (noFail, type (:~>) (..))
import Control.Monad (guard)
import Data.Bifunctor (second)
import Data.Distributive (Distributive (distribute))
import Data.Maybe (mapMaybe)
import Linear ((!*))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

-- |
--
-- [xa xb] [a] = [xc]
-- [ya yb] [b] = [yc]
getPrize :: V2 Point -> Point -> Maybe Int
getPrize coeff targ = do
  (det, invCoeff) <- inv22Int (distribute coeff)
  let resDet = invCoeff !* targ
      residues = (`mod` det) <$> resDet
      V2 a b = (`div` det) <$> resDet
  guard $ all (== 0) residues
  pure $ 3 * a + b

day13a :: [(V2 Point, Point)] :~> Int
day13a =
  MkSol
    { sParse = parseMaybe' $ flip P.sepBy "\n\n" do
        coeff <- traverse parseButton $ V2 "A" "B"
        "Prize: "
        p <- sequenceSepBy ((*>) <$> V2 "X=" "Y=" <*> V2 pDecimal pDecimal) ", "
        pure (coeff, p)
    , sShow = show
    , sSolve =
        noFail $ sum . mapMaybe (uncurry getPrize)
    }
  where
    parseButton c = do
      "Button " *> c *> ": "
      b <- sequenceSepBy ((*>) <$> V2 "X+" "Y+" <*> V2 pDecimal pDecimal) ", "
      P.newline
      pure b

day13b :: [(V2 Point, Point)] :~> Int
day13b =
  day13a
    { sSolve = sSolve day13a . map (second (10000000000000 +))
    }
