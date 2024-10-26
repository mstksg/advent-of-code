-- |
-- Module      : AOC2020.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
module AOC2020.Day02 (
  day02a,
  day02b,
)
where

import AOC.Common (CharParser, countTrue, parseLines)
import AOC.Solver ((:~>) (..))
import Control.DeepSeq (NFData)
import Control.Monad.Combinators (some)
import GHC.Generics (Generic)
import Text.Megaparsec (anySingle)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal)

data Policy = P
  { pIx1 :: Int
  , pIx2 :: Int
  , pChar :: Char
  , pPass :: String
  }
  deriving stock (Show, Eq, Ord, Generic)

instance NFData Policy

policy :: CharParser Policy
policy =
  P
    <$> decimal
    <*> (char '-' *> decimal)
    <*> (space *> anySingle)
    <*> (char ':' *> space *> some anySingle)

validate1 :: Policy -> Bool
validate1 P{..} = n >= pIx1 && n <= pIx2
  where
    n = countTrue (== pChar) pPass

validate2 :: Policy -> Bool
validate2 P{..} = n == 1
  where
    n = countTrue (== pChar) [pPass !! (pIx1 - 1), pPass !! (pIx2 - 1)]

day02a :: [Policy] :~> Int
day02a =
  MkSol
    { sParse = parseLines policy
    , sShow = show
    , sSolve = Just . countTrue validate1
    }

day02b :: [Policy] :~> Int
day02b =
  MkSol
    { sParse = parseLines policy
    , sShow = show
    , sSolve = Just . countTrue validate2
    }
