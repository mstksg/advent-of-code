-- |
-- Module      : AOC2020.Day25
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.  See "AOC.Solver" for the types used in this module!
module AOC2020.Day25
  ( day25a,
  )
where

import AOC.Common (_ListTup)
import AOC.Solver ((:~>) (..))
import Control.Lens (preview)
import Control.Monad ((<=<))
import Data.Maybe (fromJust)
import Math.NumberTheory.Moduli (Mod, PrimitiveRoot, discreteLogarithm, getVal, isMultElement, isPrimitiveRoot, (^%))
import Math.NumberTheory.Moduli.Singleton (CyclicGroup, cyclicGroup)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

type Magic = 20201227

magicGroup :: CyclicGroup Integer Magic
magicGroup = fromJust cyclicGroup

primBase :: PrimitiveRoot Magic
primBase = fromJust $ isPrimitiveRoot magicGroup 7

findSecret :: Mod Magic -> Maybe Natural
findSecret =
  fmap (discreteLogarithm magicGroup primBase)
    . isMultElement

day25a :: (Mod Magic, Mod Magic) :~> Integer
day25a =
  MkSol
    { sParse =
        preview _ListTup
          <=< traverse (fmap fromInteger . readMaybe)
            . lines,
      sShow = show,
      sSolve = \(x, y) -> getVal . (y ^%) <$> findSecret x
    }
