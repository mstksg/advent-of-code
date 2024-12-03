{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day03 (
  day03a,
  day03b,
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
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

parseMul :: CharParser Int
parseMul = do 
  "mul("
  x <- pDecimal
  ","
  y <- pDecimal
  ")"
  pure (x*y)

day03a :: [Int] :~> _
day03a =
  MkSol
    { sParse = Just . mapMaybe (parseMaybe' parseMul) . tails
    , sShow = show
    , sSolve =
        noFail $
          sum
    }

-- secMay f (x, y) = (x,) <$> f y

day03b :: _ :~> _
day03b =
  MkSol
    { sParse = \xs -> Just $ 
        let muls = mapMaybe (traverse $ parseMaybe' parseMul) . zip [0..] . tails $ xs
            dos = map fst . filter (("do()" `isPrefixOf`) . snd) . zip [0..] . tails $ xs
            donts = map fst . filter (("don't()" `isPrefixOf`) . snd) . zip [0..] . tails $ xs
         in (muls, dos, donts)
    , sShow = show
    , sSolve = noFail \(muls, dos, donts) ->
      let dodontMap = M.fromSet (const True) (S.fromList dos) <> M.fromSet (const False) (S.fromList donts)
          good i = case M.lookupLE i dodontMap of
                      Nothing -> False
                      Just (_, t) -> t
       in sum . map snd $ filter (good . fst) muls
        
        -- noFail $
        --   id
    }
