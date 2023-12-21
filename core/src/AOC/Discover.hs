{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Module      : AOC.Discover
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Template Haskell for discovering all named challenges in a given
-- directory.
module AOC.Discover
  ( mkChallengeMap,
    solutionList,
    ChallengeMap,
    ChallengeSpec (..),
    lookupSolution,
    solSpec,
    solSpecStr,
    solSpecStr_,
    charPart,
    specSomeSol,
    liftDay,
    liftPart,
  )
where

import AOC.Solver
import Advent
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.Function
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable
import Data.Void
import GHC.Exts
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Datatype
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PL
import Text.Printf
import Prelude

-- | Big quick escape hatch if things explode in the middle of solving.
-- This will disable the check for NFData when using 'MkSomeSol' and assume
-- no NFData in every case.
checkIfNFData :: Bool
checkIfNFData = True

-- checkIfNFData = False

-- | A specification for a specific challenge.  Should consist of a day and
-- a lowercase character.
data ChallengeSpec = CS
  { _csDay :: Day,
    _csPart :: Part
  }
  deriving stock (Show, Eq, Ord)

-- | A map of days to parts to solutions.
type ChallengeMap = Map Day (Map Part SomeSolution)

-- | Lookup up a solution from a 'ChallengeMap'
lookupSolution :: ChallengeSpec -> Map Day (Map Part a) -> Maybe a
lookupSolution CS {..} = M.lookup _csPart <=< M.lookup _csDay

-- | Get a 'ChallengeSpec' from a given reified solution (name).
--
-- @
-- solSpec \'day02a == CS { _csDay = 1, _csPart = 'a' }
-- @
solSpec :: TH.Name -> ChallengeSpec
solSpec = solSpecStr_ . TH.nameBase

solSpecStr :: String -> Either (P.ParseErrorBundle String Void) ChallengeSpec
solSpecStr = P.runParser challengeName ""

solSpecStr_ :: String -> ChallengeSpec
solSpecStr_ = either (error . P.errorBundlePretty) id . solSpecStr

type Parser = P.Parsec Void String

-- | Template Haskell splice to produce a list of all named solutions in
-- scope. Expects solutions as function names following the format
-- @dayDDp@, where @DD@ is a two-digit zero-added day, and @p@ is
-- a lower-case letter corresponding to the part of the challenge.
--
-- See 'mkChallengeMap' for a description of usage.
solutionList :: TH.Code TH.Q [(Day, (Part, SomeSolution))]
solutionList =
  TH.Code $
    TH.TExp . TH.ListE . catMaybes
      <$> traverse (fmap (fmap TH.unType) . specExp) (S.toList challengeSpecUniverse)

-- | Meant to be called like:
--
-- @
-- mkChallengeMap $$(solutionList)
-- @
mkChallengeMap :: [(Day, (Part, SomeSolution))] -> ChallengeMap
mkChallengeMap =
  M.unionsWith M.union
    . map (uncurry M.singleton . second (uncurry M.singleton))

challengeSpecUniverse :: Set ChallengeSpec
challengeSpecUniverse =
  S.delete (CS (mkDay_ 25) Part2) . S.fromList $
    CS <$> [minBound .. maxBound] <*> [minBound .. maxBound]

-- | Looks up the name in scope
specExp :: ChallengeSpec -> TH.Q (Maybe (TH.TExp (Day, (Part, SomeSolution))))
specExp s@(CS d p) = do
  mn <- TH.lookupValueName (specName s)
  for mn \n -> do
    ss <- TH.unTypeCode $ specSomeSol n
    pure $
      TH.TExp $
        TH.TupE
          [ Just . TH.unType $ liftDay d,
            Just $
              TH.TupE
                [ Just . TH.unType $ liftPart p,
                  Just ss
                ]
          ]

-- | Looks up the name in scope
specSomeSol :: TH.Name -> TH.Code TH.Q SomeSolution
specSomeSol n = TH.Code do
  isNF <- solverNFData n
  let con
        | isNF = 'MkSomeSolNF
        | otherwise = 'MkSomeSolWH
  pure $ TH.TExp $ TH.ConE con `TH.AppE` TH.VarE n

liftDay :: Day -> TH.TExp Day
liftDay d = TH.TExp $ TH.VarE 'mkDay_ `TH.AppE` TH.LitE (TH.IntegerL (dayInt d))

liftPart :: Part -> TH.TExp Part
liftPart = \case
  Part1 -> TH.TExp $ TH.ConE 'Part1
  Part2 -> TH.TExp $ TH.ConE 'Part2

specName :: ChallengeSpec -> String
specName (CS d p) = printf "day%02d%c" (dayInt d) (partChar p)

challengeName :: Parser ChallengeSpec
challengeName = do
  _ <- P.string "day"
  dInt <- PL.decimal
  dFin <-
    maybe (fail $ "Day not in range: " ++ show dInt) pure $
      mkDay dInt
  c <- P.lowerChar
  p <-
    maybe (fail $ printf "Part not parsed: %c" c) pure $
      charPart c
  pure $ CS dFin p

-- | Parse a 'Char' into a 'Part'
charPart :: Char -> Maybe Part
charPart 'a' = Just Part1
charPart 'b' = Just Part2
charPart _ = Nothing

-- | Check if a solver identifier is of type @A ':~>' B@, where @B@ is an
-- instance of 'NFData'.
solverNFData :: TH.Name -> TH.Q Bool
solverNFData n
  | checkIfNFData =
      TH.reify n >>= \case
        TH.VarI _ (TH.ConT c `TH.AppT` a `TH.AppT` _) _
          | c == ''(:~>) -> deepInstance ''NFData a
        _ -> pure False
  | otherwise = pure False

-- | Check if a type is an instance of a class, unifying when possible
deepInstance ::
  -- | class
  TH.Name ->
  -- | type
  TH.Type ->
  TH.Q Bool
deepInstance cn = fmap isJust . runMaybeT . deepInstance_ cn

deepInstance_ ::
  -- | class
  TH.Name ->
  -- | type
  TH.Type ->
  MaybeT TH.Q ()
deepInstance_ cn t = do
  insts <- maybe empty pure . NE.nonEmpty =<< lift (TH.reifyInstances cn [t])
  forM_ insts $ \case
    TH.InstanceD _ ctx instHead _ -> do
      uni <- lift $ unifyTypes [TH.ConT cn `TH.AppT` t, instHead]
      forM_ ctx $ \case
        TH.AppT (TH.ConT c) v -> deepInstance_ c (applySubstitution uni v)
        _ -> empty
    _ -> empty
