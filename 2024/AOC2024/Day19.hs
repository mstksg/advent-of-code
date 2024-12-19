{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day19 (
day19a,
day19b

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

data CharTrie a = CT { ctHere :: Maybe a, ctThere :: Map Char (CharTrie a) }
  deriving stock (Show, Functor, Traversable, Foldable, Generic)

deriving anyclass instance NFData a => NFData (CharTrie a)

instance Semigroup a => Semigroup (CharTrie a) where
  CT h1 t1 <> CT h2 t2 = CT (h1 <> h2) (M.unionWith (<>) t1 t2)

instance Semigroup a => Monoid (CharTrie a) where
  mempty = CT Nothing M.empty

joinTrie :: (Monoid a) => CharTrie (CharTrie a) -> CharTrie a
joinTrie CT{..} = case ctHere of
  Nothing -> CT Nothing $ joinTrie <$> ctThere
  Just (CT here' there') -> CT (here' <> Just mempty) (M.unionWith (<>) there' (joinTrie <$> ctThere))

singletonTrie :: String -> a -> CharTrie a
singletonTrie = \case
  [] -> \x -> CT (Just x) M.empty
  c:cs -> CT Nothing . M.singleton c . singletonTrie cs

lookupTrie :: String -> CharTrie a -> Maybe a
lookupTrie = \case
  [] -> \CT{..} -> ctHere
  c:cs -> \CT{..} -> do
    t' <- M.lookup c ctThere
    lookupTrie cs t'

day19a :: _ :~> _
day19a =
  MkSol
    { sParse =
        parseMaybe' do
          ws <- pAlphaNumWord `P.sepBy` ","
          P.newline
          P.newline
          ls <- pAlphaNumWord `P.sepBy` P.newline
          pure (ws, ls)
    , sShow = show
    , sSolve =
        noFail \(ws,ls) -> 
          let wtrie = foldMap (`singletonTrie` ()) ws
              infiniTrie :: CharTrie ()
              infiniTrie = joinTrie $ infiniTrie <$ wtrie
           -- in isJust . ctHere <$> ctThere (infiniTrie)
           in countTrue (isJust . flip lookupTrie infiniTrie) ls
          -- let eat = P.try . P.choice $ ([] <$ P.try P.eof):
          --           [ P.try do w' <- P.try (P.string w)
          --                      (w:) <$> P.try eat
          --             | (i, w) <- zip [0..] ws
          --           ]
          --  in countTrue (traceShowId . isJust . parseMaybe' eat) ls
          -- let eat = undefined
          --  in countTrue (eat) ls
           -- in map (parseMaybe' eat) ls
    }

day19b :: _ :~> _
day19b =
  MkSol
    { sParse = sParse day19a
    , sShow = show
    , sSolve =
        noFail \(ws,ls) -> 
          let wtrie :: CharTrie (Sum Int)
              wtrie = foldMap (`singletonTrie` 1) ws
              infiniTrie :: CharTrie (Sum Int)
              infiniTrie = joinTrie $ wtrie <&> \_ -> singletonTrie "" 1 <> infiniTrie
           -- in wtrie
           -- in joinTrie $ wtrie <&> \_ -> wtrie
           -- in fmap ctHere . ctThere <$> ctThere infiniTrie
           in sum . map (getSum . fold . flip lookupTrie infiniTrie) $ ls
           -- in sum . map (getSum . fold . flip lookupTrie infiniTrie) $ ls
    }
