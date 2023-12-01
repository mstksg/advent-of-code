{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import Data.List (tails, isPrefixOf)
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

day01a :: _ :~> _
day01a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . sum . mapMaybe (readMaybe @Int . (\xs -> [head xs, last xs]) . filter isDigit)
    }

day01b :: _ :~> _
day01b = MkSol
    { sParse = sParse day01a
    , sShow  = show
    , sSolve = Just
             . sum
             . map (((\xs -> (head xs :: Int)*10 + last xs)) . mapMaybe hasNumber . tails)
    }
  where
    hasNumber x = firstJust (\(t,y) -> guard (t `isPrefixOf` x) $> y) $
        [ (show y, y) | y <- [0..9] ]
     ++ ( zip ["zero","one","two","three","four","five","six","seven","eight","nine"] [0..9])
