{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day17 (
day17a,
day17b
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
import Data.Bits
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map as M
import qualified Data.Map.NonEmpty as NEM
import qualified Data.OrdPSQ as PSQ
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

day17a :: _ :~> _
day17a =
  MkSol
    { sParse = parseMaybe' do
        a <- "Register A: " *> pDecimal
        P.newline
        b <- "Register B: " *> pDecimal
        P.newline
        c <- "Register C: " *> pDecimal
        P.newline
        P.newline
        d <- "Program: " *> (pDecimal `sepBy'` ",")
        pure (a,b,c,d)
    , sShow = intercalate "," . map show
    , sSolve =
        noFail $ \(a,b,c,p :: [Int]) ->
              go 0 (V3 a b c) (Seq.fromList p)
    }

go :: Int -> V3 Int -> Seq Int -> [Int]
go i (V3 a b c) tp = traceShow (i, V3 a b c) $ case (,) <$> Seq.lookup i tp <*> Seq.lookup (i + 1) tp of
  Nothing -> []
  Just (q,o) ->
    let x = case o of
              0 -> 0
              1 -> 1
              2 -> 2
              3 -> 3
              4 -> a
              5 -> b
              6 -> c
    in case q of
         0 -> go (i + 2) (V3 (a `div` (2^x)) b c) tp
         1 -> go (i + 2) (V3 a (b `xor` o) c) tp
         2 -> go (i + 2) (V3 a (x `mod` 8) c) tp
         3 | a == 0 -> go (i + 2) (V3 a b c) tp
           | otherwise -> go o (V3 a b c) tp
         4 -> go (i + 2) (V3 a (b `xor` c) c) tp
         5 -> trace (show (x `mod` 8, o, x)) (x `mod` 8) : go (i + 2) (V3 a b c) tp
         6 -> go (i + 2) (V3 a (a `div` (2^x)) c) tp
         7 -> go (i + 2) (V3 a b (a `div` (2^x))) tp


-- 2,4, 1,6, 7,5, 4,6, 1,4, 5,5, 0,3, 3,0
--
-- BST A    --- b = (a `mod` 8)
-- BXL 6    --- b ^= 110    (6)
-- CDV B    --- c = a / (2^b)
-- BXC      --- b ^= c
-- BXL 4    --- b ^= 100    (4)
-- OUT B    --- print b
-- ADV 3    --- a /= 8
-- JNZ 0

-- The *`adv`* instruction (opcode *`0`*) performs *division*. The
-- numerator is the value in the `A` register. The denominator is found by
-- raising 2 to the power of the instruction's *combo* operand. (So, an
-- operand of `2` would divide `A` by `4` (`2^2`); an operand of `5` would
-- divide `A` by `2^B`.) The result of the division operation is
-- *truncated* to an integer and then written to the `A` register.

-- The *`bxl`* instruction (opcode *`1`*) calculates the [bitwise
-- XOR](https://en.wikipedia.org/wiki/Bitwise_operation#XOR){target="_blank"}
-- of register `B` and the instruction's *literal* operand, then stores the
-- result in register `B`.

-- The *`bst`* instruction (opcode *`2`*) calculates the value of its
-- *combo* operand
-- [modulo](https://en.wikipedia.org/wiki/Modulo){target="_blank"} 8
-- (thereby keeping only its lowest 3 bits), then writes that value to the
-- `B` register.

-- The *`jnz`* instruction (opcode *`3`*) does *nothing* if the `A`
-- register is `0`. However, if the `A` register is *not zero*, it
-- [*jumps*]{title="The instruction does this using a little trampoline."}
-- by setting the instruction pointer to the value of its *literal*
-- operand; if this instruction jumps, the instruction pointer is *not*
-- increased by `2` after this instruction.

-- The *`bxc`* instruction (opcode *`4`*) calculates the *bitwise XOR* of
-- register `B` and register `C`, then stores the result in register `B`.
-- (For legacy reasons, this instruction reads an operand but *ignores*
-- it.)

-- The *`out`* instruction (opcode *`5`*) calculates the value of its
-- *combo* operand modulo 8, then *outputs* that value. (If a program
-- outputs multiple values, they are separated by commas.)

-- The *`bdv`* instruction (opcode *`6`*) works exactly like the `adv`
-- instruction except that the result is stored in the *`B` register*. (The
-- numerator is still read from the `A` register.)

-- The *`cdv`* instruction (opcode *`7`*) works exactly like the `adv`
-- instruction except that the result is stored in the *`C` register*. (The
-- numerator is still read from the `A` register.)
-- --

day17b :: _ :~> _
day17b =
  MkSol
    { sParse = sParse day17a
    , sShow = show
    -- , sShow = intercalate "\n" . map show
    , sSolve =
        noFail $ \(_,_,_,p :: [Int]) ->
          execState (stepBackwards (reverse p)) (V3 0 0 0)
          -- [ (go 0 (V3 i b c) (Seq.fromList p))
          -- -- | i <- [45184372088832]
          -- | i <- [1999]
          -- ]
    }

stepBackwards :: [Int] -> State (V3 Int) ()
stepBackwards = \case
  [] -> pure ()   -- still need to progress back up to the first
  o:os -> do
    -- oh no, this is wrong because we only get o mod 8
    -- _y .= (o `xor` 6)
    let lsbA = [ a
                | a <- [0..1000]
               , let b0 = (a .&. 7) `xor` 6
               , (b0 `xor` (a `shift` (-b0))) .&. 7 == o
              ]
    traceM $ show (o, lsbA)
    pure ()

-- CDV B    --- c = a / (2^b)
-- BXC      --- b ^= c
--
-- c = a / (2^b0)
-- b = b0 ^ c
--
-- b = b0 ^ (a >> b0)
--
-- b = (b0 ^ 110) ^ (a >> (b0 ^ 110))
--
-- b = ((a `mod` 8) ^ 110) ^ (a >> ((a `mod` 8) ^ 110))
-- 
-- mod 8 is & 111
--
-- b & 111 = ((a & 111) ^ 110) ^ (a >> ((a & 111) ^ 110))
-- b & 111 = ((a ^ 110) & 111) ^ (a >> ((a ^ 110) & 111))
--
-- i guess we can do a search for what (a & 111) is, it's only 7 possible
-- items. But how do we get from that to the full A?
--
-- Ah hah i guess we can store the rest of the A trit-by-trit.
--
--
--

-- stepBackwards r@(V3 a b c) = \case
--   [] -> r
--   x:xs ->

-- BST A    --- b = (a `mod` 8)
-- BXL 6    --- b ^= 110    (6)
-- CDV B    --- c = a / (2^b)
-- BXC      --- b ^= c
-- BXL 4    --- b ^= 100    (4)
-- OUT B    --- print (b `mod` 8)
-- ADV 3    --- a /= 8
-- JNZ 0
