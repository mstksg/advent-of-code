-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
module AOC2023.Day19 (
  day19a,
  day19b,
)
where

import AOC.Common (countTrue)
import AOC.Common.Parser (
  CharParser,
  fullLine,
  manyTillWithout,
  pDecimal,
  parseMaybe',
  sepBy',
  tokenAssoc,
  tokenMap,
 )
import AOC.Solver (noFail, (:~>) (..))
import Control.Applicative (Alternative ((<|>)))
import Control.DeepSeq (NFData)
import Data.Either (partitionEithers)
import Data.Functor.Foldable (hylo)
import Data.Interval (Interval)
import qualified Data.Interval as IV
import Data.IntervalMap.Strict (IntervalMap)
import qualified Data.IntervalMap.Strict as IVM
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IVS
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data XMAS = X | M | A | S
  deriving stock (Eq, Ord, Show, Generic, Enum)

instance NFData XMAS

xmasMap :: Map Char XMAS
xmasMap = M.fromList $ zip "xmas" [X .. S]

opMap :: Map Char Ordering
opMap = M.fromList $ zip "<=>" [LT, EQ, GT]

data Result a = Reject | Accept | Defer a
  deriving stock (Eq, Ord, Show, Generic, Functor)

instance NFData a => NFData (Result a)

result :: b -> b -> (a -> b) -> Result a -> b
result r a d = \case
  Reject -> r
  Accept -> a
  Defer x -> d x

data Rule a = Rule
  { rXmas :: XMAS
  , rOp :: Ordering
  , rVal :: Int
  , rResult :: Result a
  }
  deriving stock (Eq, Ord, Show, Generic, Functor)

instance NFData a => NFData (Rule a)

data Workflow a = Workflow
  { wfRules :: [Rule a]
  , wfDefault :: Result a
  }
  deriving stock (Eq, Ord, Show, Generic, Functor)

instance NFData a => NFData (Workflow a)

workflowParser :: CharParser (String, Workflow String)
workflowParser = do
  key <- manyTillWithout P.anySingle "{"
  workflow <- P.between "{" "}" do
    (wfRules, wfDefault : _) <-
      partitionEithers <$> sepBy' (Left <$> parseRule <|> Right <$> parseResult) ","
    pure Workflow{..}
  pure (key, workflow)
  where
    parseRule :: CharParser (Rule String)
    parseRule = P.try do
      rXmas <- tokenMap xmasMap
      rOp <- tokenMap opMap
      rVal <- pDecimal
      ":"
      rResult <- parseResult
      pure Rule{..}
    parseResult :: CharParser (Result String)
    parseResult = tokenAssoc [('R', Reject), ('A', Accept)] <|> (Defer <$> P.many P.letterChar)

bagParser :: CharParser (Map XMAS Int)
bagParser = fmap M.fromList . P.between "{" "}" . flip sepBy' "," $ do
  x <- tokenMap xmasMap
  "="
  (x,) <$> pDecimal

evalWorkflow :: Map XMAS Int -> Workflow Bool -> Bool
evalWorkflow mp = go
  where
    go Workflow{..} = foldr eval (unResult wfDefault) wfRules
    eval Rule{..} rest
      | compare (mp M.! rXmas) rVal == rOp = unResult rResult
      | otherwise = rest
    unResult = result False True id

day19a :: (Map String (Workflow String), [Map XMAS Int]) :~> Int
day19a =
  MkSol
    { sParse = parseMaybe' do
        workflows <- M.fromList <$> P.many (fullLine workflowParser)
        "\n"
        bags <- P.many (fullLine bagParser)
        pure (workflows, bags)
    , sShow = show
    , sSolve = noFail $ \(wfs, xs) ->
        sum
          . map sum
          . filter (\x -> hylo (evalWorkflow x) (wfs M.!) "in")
          $ xs
    }

xmasRange :: Interval Int
xmasRange = IV.Finite 1 IV.<=..<= IV.Finite 4000

newtype XmasSet = XmasSet (IntervalMap Int (IntervalMap Int (IntervalMap Int (IntervalSet Int))))
  deriving stock (Eq, Show, Generic)

instance NFData XmasSet

-- | Optimization to merge connected map entries together if their values are
-- equal
reMap :: (Ord k, Eq a) => IntervalMap k a -> IntervalMap k a
reMap = IVM.fromList . eat . IVM.toAscList
  where
    eat [] = []
    eat ((i, v) : xs) = go i v xs
    go i v = \case
      [] -> [(i, v)]
      (j, u) : xs
        | i `IV.isConnected` j && v == u -> go (i `IV.hull` j) v xs
        | otherwise -> (i, v) : go j u xs

intersect :: XmasSet -> XmasSet -> XmasSet
intersect (XmasSet xs) (XmasSet xs') =
  XmasSet $ subInter (subInter (subInter IVS.intersection)) xs xs'
  where
    subInter f x = reMap . IVM.intersectionWith f x

union :: XmasSet -> XmasSet -> XmasSet
union (XmasSet xs) (XmasSet xs') =
  XmasSet $ subUnion (subUnion (subUnion IVS.union)) xs xs'
  where
    subUnion f x = reMap . IVM.unionWith f x

-- ....x   .....   ....x
-- .x..x   x...x   .x...
-- .xx.x   xx.x.   ..x.x
-- x..x.   .xx.x   x..x.
--
-- combine A^B and whatever is in A but not B
difference :: XmasSet -> XmasSet -> XmasSet
difference (XmasSet xs) (XmasSet xs') =
  XmasSet $
    diffWith (diffWith (diffWith IVS.difference)) xs xs'
  where
    diffWith f a b = reMap $ IVM.intersectionWith f a b <> IVM.difference a b

size :: XmasSet -> Int
size (XmasSet xs) = (sumBySize . sumBySize . sumBySize) (sum . map ivalSize . IVS.toList) xs
  where
    sumBySize f = sum . map (\(i, a) -> ivalSize i * f a) . IVM.toList
    ivalSize i =
      IV.width i
        - 1
        + countTrue (== IV.Closed) (map snd [IV.lowerBound' i, IV.upperBound' i])

xmasRule :: Rule XmasSet -> XmasSet -> XmasSet
xmasRule Rule{..} rest = case rResult of
  Reject -> rest `difference` ivalXmas
  Accept -> ivalXmas `union` rest
  Defer s -> (s `intersect` ivalXmas) `union` (rest `difference` ivalXmas)
  where
    ival = case rOp of
      LT -> IV.Finite 1 IV.<=..< IV.Finite rVal
      GT -> IV.Finite rVal IV.<..<= IV.Finite 4000
      EQ -> IV.singleton rVal
    ivalXmas = case rXmas of
      X ->
        XmasSet $
          IVM.singleton ival . IVM.singleton xmasRange . IVM.singleton xmasRange $
            IVS.singleton xmasRange
      M ->
        XmasSet $
          IVM.singleton xmasRange . IVM.singleton ival . IVM.singleton xmasRange $
            IVS.singleton xmasRange
      A ->
        XmasSet $
          IVM.singleton xmasRange . IVM.singleton xmasRange . IVM.singleton ival $
            IVS.singleton xmasRange
      S ->
        XmasSet $
          IVM.singleton xmasRange . IVM.singleton xmasRange . IVM.singleton xmasRange $
            IVS.singleton ival

workflowInterval :: Workflow XmasSet -> XmasSet
workflowInterval Workflow{..} = foldr xmasRule (unResult wfDefault) wfRules
  where
    unResult = result noXmas allXmas id
    allXmas =
      XmasSet
        . IVM.singleton xmasRange
        . IVM.singleton xmasRange
        . IVM.singleton xmasRange
        $ IVS.singleton xmasRange
    noXmas = XmasSet IVM.empty

day19b :: Map String (Workflow String) :~> Int
day19b =
  MkSol
    { sParse = parseMaybe' $ M.fromList <$> P.many (fullLine workflowParser)
    , sShow = show
    , sSolve =
        noFail $ \wfs ->
          size $ hylo workflowInterval (wfs M.!) "in"
    }
