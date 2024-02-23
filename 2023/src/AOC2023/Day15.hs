-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
module AOC2023.Day15 (
  day15a,
  day15b,
)
where

import AOC.Common (digitToIntSafe)
import AOC.Solver (noFail, (:~>) (..))
import AOC.Util (strip)
import Control.DeepSeq (NFData)
import Control.Lens (makeLenses, set, unsnoc)
import Data.Char (ord)
import Data.Functor ((<&>))
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEM
import Data.Maybe (fromJust)
import Data.Monoid (Any (..))
import GHC.Generics (Generic)
import Safe (initMay)

hasher :: [Char] -> Int
hasher = foldl' go 0
  where
    go curr c = ((curr + ord c) * 17) `mod` 256

day15a :: [String] :~> Int
day15a =
  MkSol
    { sParse =
        noFail $
          splitOn ","
    , sShow = show
    , sSolve =
        noFail $
          sum . map hasher
    }

data Act = Delete | Set Int
  deriving stock (Eq, Ord, Show, Generic)

instance NFData Act

parseAct :: String -> Maybe (String, Act)
parseAct str = do
  (allButLast, sig) <- unsnoc str
  case sig of
    '-' -> pure (allButLast, Delete)
    d -> do
      n <- digitToIntSafe d
      label <- initMay allButLast
      pure (label, Set n)

data BoxNode = BN
  { _bnValue :: !Int
  , _bnAfter :: !(Maybe String)
  , _bnBefore :: !(Maybe String)
  }
  deriving stock (Eq, Ord, Show, Generic)

instance NFData BoxNode

makeLenses ''BoxNode

data Box = B
  { _bMap :: NEMap String BoxNode
  , _bFirst :: String
  , _bLast :: String
  }
  deriving stock (Eq, Ord, Show, Generic)

instance NFData Box

deleteBox :: String -> Box -> Maybe Box
deleteBox label b = case NEM.alterF (,Nothing) label (_bMap b) of
  (Nothing, _) -> Just b
  (Just bn, mp) ->
    NEM.nonEmptyMap mp <&> \mp' ->
      let mp'' =
            maybe id (NEM.adjust (set bnBefore (_bnBefore bn))) (_bnAfter bn)
              . maybe id (NEM.adjust (set bnAfter (_bnAfter bn))) (_bnBefore bn)
              $ mp'
       in B
            { _bMap = mp''
            , _bFirst = if _bFirst b == label then fromJust (_bnAfter bn) else _bFirst b
            , _bLast = if _bLast b == label then fromJust (_bnBefore bn) else _bLast b
            }

setBox :: String -> Int -> Box -> Box
setBox label n b
  | isNew =
      B
        { _bMap = NEM.adjust (set bnAfter (Just label)) (_bLast b) mp
        , _bFirst = _bFirst b
        , _bLast = label
        }
  | otherwise = b{_bMap = mp}
  where
    (Any isNew, mp) = NEM.alterF' go label (_bMap b)
    go = \case
      Nothing -> (Any True, BN n Nothing (Just $ _bLast b))
      Just bn -> (Any False, set bnValue n bn)

initBox :: String -> Int -> Box
initBox label n = B (NEM.singleton label (BN n Nothing Nothing)) label label

boxSum :: Box -> Int
boxSum B{..} = go 1 0 _bFirst
  where
    go !i !n x = case _bnAfter of
      Nothing -> n'
      Just y -> go (i + 1) n' y
      where
        n' = n + _bnValue * i
        BN{..} = _bMap NEM.! x

day15b :: [(String, Act)] :~> Int
day15b =
  MkSol
    { sParse = traverse parseAct . splitOn "," . strip
    , sShow = show
    , sSolve =
        noFail $
          sum . IM.mapWithKey score . foldl' go IM.empty
    }
  where
    go mp (lbl, act) = case act of
      Delete -> IM.update (deleteBox lbl) i mp
      Set n -> IM.alter (Just . maybe (initBox lbl n) (setBox lbl n)) i mp
      where
        i = hasher lbl + 1
    score boxNum box = boxNum * boxSum box
