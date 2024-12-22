{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day22
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day22 (
  day22a,
  day22b,
)
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntMap.NonEmpty as IM
import qualified Data.IntSet as IS
import qualified Data.IntSet.NonEmpty as NEIS
import qualified Data.List.NonEmpty as NE
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map as M
import qualified Data.Map.NonEmpty as NEM
import qualified Data.OrdPSQ as PSQ
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as S
import Data.Bits
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

day22a :: _ :~> _
day22a =
  MkSol
    { sParse = parseMaybe' $ 
          sepByLines pDecimal
        -- noFail $
        --   lines
    , sShow = show
    , sSolve =
        noFail $
          sum . map ((!! 2000) . iterate step)
    }

step :: Int -> Int
step n = n'''
  where
    n' = prune $ (n*64) `xor` n
    n'' = prune $ (n' `div` 32) `xor` n'
    n''' = prune $ (n'' * 2048) `xor` n''
    prune = (`mod` 16777216)

-- -   Calculate the result of *multiplying the secret number by `64`*.
--     Then, *mix* this result into the secret number. Finally, *prune* the
--     secret number.
-- -   Calculate the result of *dividing the secret number by `32`*. Round
--     the result down to the nearest integer. Then, *mix* this result into
--     the secret number. Finally, *prune* the secret number.
-- -   Calculate the result of *multiplying the secret number by `2048`*.
--     Then, *mix* this result into the secret number. Finally, *prune* the
--     secret number.

-- Each step of the above process involves *mixing* and *pruning*:

-- -   To *mix* a value into the secret number, calculate the [bitwise
--     XOR](https://en.wikipedia.org/wiki/Bitwise_operation#XOR){target="_blank"}
--     of the given value and the secret number. Then, the secret number
--     becomes the result of that operation. (If the secret number is `42`
--     and you were to *mix* `15` into the secret number, the secret number
--     would become `37`.)
-- -   To *prune* the secret number, calculate the value of the secret
--     number
--     [modulo](https://en.wikipedia.org/wiki/Modulo){target="_blank"}
--     `16777216`. Then, the secret number becomes the result of that
--     operation. (If the secret number is `100000000` and you were to
--     *prune* the secret number, the secret number would become
--     `16113920`.)

day22b :: _ :~> _
day22b =
  MkSol
    { sParse = sParse day22a
    , sShow = show
    , sSolve =
        noFail $ \xs ->
          let serieses = xs <&> \x ->
                let ps = take 2000 $ map (`mod` 10) $ iterate step x 
                    dPs = zipWith (\p0 p1 -> (p1, p1 - p0)) ps (drop 1 ps)
                    windows = slidingWindows 4 dPs <&> \w -> (snd <$> w, fst $ last (toList w))
                    seqMap = M.fromListWith (flip const) windows
                in seqMap
              bests = M.unionsWith (+) serieses
           in snd $ maximumBy (comparing snd) (M.toList bests)
              -- bests = M.unionsWith (<>) $ map (fmap (:[])) serieses
           -- in maximumBy (comparing (sum . snd)) (M.toList bests)
    }
  where
    diffs xs = zipWith subtract xs (drop 1 xs)
