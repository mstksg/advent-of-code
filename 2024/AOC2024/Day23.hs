-- |
-- Module      : AOC2024.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day23 (
  day23a,
  day23b,
)
where

import AOC.Common (countTrue)
import AOC.Common.Parser (pAlphaNumWord, parseMaybe', sepByLines, sequenceSepBy)
import AOC.Solver (noFail, type (:~>) (..))
import Control.Monad (guard)
import Data.Foldable (Foldable (toList))
import Data.Functor.Foldable (hylo)
import Data.List (intercalate, isPrefixOf, sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics ((:.:) (..))
import Linear (V2 (..), V3 (..))
import Safe.Foldable (maximumByMay)

connMap :: Ord a => [V2 a] -> Map a (Set a)
connMap xs =
  M.unionsWith
    (<>)
    [ M.fromList [(a, S.singleton b), (b, S.empty)]
    | [a, b] <- sort . toList <$> xs
    ]

day23a :: [V2 String] :~> Int
day23a =
  MkSol
    { sParse = parseMaybe' $ sepByLines $ sequenceSepBy (V2 pAlphaNumWord pAlphaNumWord) "-"
    , sShow = show
    , sSolve =
        noFail \xs ->
          let conns = connMap xs
           in countTrue (any ("t" `isPrefixOf`)) do
                (a, bs) <- M.toList conns
                b <- toList bs
                c <- toList $ conns M.! b
                guard $ c `S.member` bs
                pure (V3 a b c)
    }

day23b :: [V2 String] :~> [String]
day23b =
  MkSol
    { sParse = sParse day23a
    , sShow = intercalate ","
    , sSolve = \xs -> do
        let conns = connMap xs
        maximumByMay @[] (comparing length) $
          hylo @([] :.: (,) String)
            (foldMap (\(here, there) -> (here :) <$> if null there then pure [] else there) . unComp1)
            ( fmap \cands ->
                Comp1
                  [ (b, cands `S.intersection` (conns M.! b))
                  | b <- toList cands
                  ]
            )
            (Comp1 $ M.toList conns)
    }
