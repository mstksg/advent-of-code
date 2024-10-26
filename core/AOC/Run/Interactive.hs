{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : AOC.Run.Interactive
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Versions of loaders and runners meant to be used in GHCI.
module AOC.Run.Interactive (
  -- * Fetch and Run
  fromSol,

  -- ** Return Answers
  execSolution,
  lockSolution,
  execSolutionWith,
  testSolution,
  testSolutionOnly,
  viewPrompt,
  waitForPrompt,
  submitSolution,

  -- ** No Answers
  execSolution_,
  lockSolution_,
  execSolutionWith_,
  testSolution_,
  testSolutionOnly_,
  viewPrompt_,
  waitForPrompt_,
  submitSolution_,

  -- * Load Inputs
  loadInput,
  loadParseInput,
  loadTests,
  loadParseTests,

  -- * Util
  RunInteractive (..),
  mkSpec,
)
where

import AOC.Discover
import AOC.Run
import AOC.Run.Config
import AOC.Run.Load
import AOC.Solver
import AOC.Util
import Advent
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Language.Haskell.TH as TH

data RunInteractive = RI
  { _riYear :: Integer
  , _riSpec :: ChallengeSpec
  , _riSolution :: SomeSolution
  }

riChallengeBundle :: RunInteractive -> ChallengeBundle
riChallengeBundle RI{..} =
  CB
    { _cbYear = _riYear
    , _cbChallengeMap =
        M.singleton
          (_csDay _riSpec)
          (M.singleton (_csPart _riSpec) _riSolution)
    }

-- | Interactively call 'mainRun'.
runInteractive ::
  MainRunOpts -> RunInteractive -> ExceptT [String] IO (Maybe Bool, Either [String] String)
runInteractive mro ri@RI{..} = do
  cfg <- liftIO $ configFile defConfPath
  out <- mainRun (riChallengeBundle ri) cfg mro
  maybeToEither ["Result not found in result map (Internal Error)"] $
    M.lookup _riSpec out

-- | Run the solution indicated by the challenge spec on the official
-- puzzle input.  Get answer as result.
execSolution :: RunInteractive -> IO String
execSolution ri@RI{..} = eitherIO do
  (_, res) <- runInteractive (defaultMRO (TSPart _riSpec)) ri
  liftEither res

-- | Run the solution indicated by the challenge spec on the official
-- puzzle input, and lock the answer as correct.  Get answer as result.
lockSolution :: RunInteractive -> IO String
lockSolution ri@RI{..} = eitherIO do
  (_, res) <-
    runInteractive
      (defaultMRO (TSPart _riSpec)){_mroLock = True}
      ri
  liftEither res

-- | Run the solution indicated by the challenge spec on a custom input.
-- Get answer as result.
execSolutionWith ::
  RunInteractive ->
  -- | custom puzzle input
  String ->
  IO String
execSolutionWith ri@RI{..} inp = eitherIO do
  (_, res) <-
    runInteractive
      (defaultMRO (TSPart _riSpec)){_mroInput = \_ -> pure $ Just inp}
      ri
  liftEither res

-- | Run test suite for a given challenge spec.
--
-- Returns 'Just' if any tests were run, with a 'Bool' specifying whether
-- or not all tests passed.
testSolution :: RunInteractive -> IO (Maybe Bool)
testSolution ri@RI{..} =
  eitherIO $
    fst <$> runInteractive (defaultMRO (TSPart _riSpec)){_mroTest = True} ri

-- | Run test suite for a given challenge spec, and NOT the actual input.
--
-- Returns 'Just' if any tests were run, with a 'Bool' specifying whether
-- or not all tests passed.
testSolutionOnly :: RunInteractive -> IO (Maybe Bool)
testSolutionOnly ri@RI{..} =
  eitherIO $
    fst <$> runInteractive (defaultMRO (TSPart _riSpec)){_mroTest = True, _mroActual = False} ri

-- | View the prompt for a given challenge spec.
viewPrompt :: RunInteractive -> IO Text
viewPrompt ri@RI{..} = eitherIO $ do
  cfg <- liftIO $ configFile defConfPath
  out <- mainView (riChallengeBundle ri) cfg . defaultMVO $ TSPart _riSpec
  maybeToEither ["Prompt not found in result map (Internal Error)"] $
    M.lookup _riSpec out

-- | Countdown to get the prompt for a given challenge spec, if not yet
-- available.
waitForPrompt :: RunInteractive -> IO Text
waitForPrompt ri@RI{..} = eitherIO $ do
  cfg <- liftIO $ configFile defConfPath
  out <-
    mainView (riChallengeBundle ri) cfg $
      (defaultMVO (TSPart _riSpec))
        { _mvoWait = True
        }
  maybeToEither ["Prompt not found in result map (Internal Error)"] $
    M.lookup _riSpec out

-- | Submit solution for a given challenge spec, and lock if correct.
submitSolution :: RunInteractive -> IO (Text, SubmitRes)
submitSolution ri@RI{..} = eitherIO $ do
  cfg <- liftIO $ configFile defConfPath
  mainSubmit (riChallengeBundle ri) cfg (defaultMSO _riSpec)

-- | Result-suppressing version of 'execSolution'.
execSolution_ :: RunInteractive -> IO ()
execSolution_ = void . execSolution

-- | Result-suppressing version of 'lockSolution'.
lockSolution_ :: RunInteractive -> IO ()
lockSolution_ = void . lockSolution

-- | Result-suppressing version of 'execSolutionWith'.
execSolutionWith_ ::
  RunInteractive ->
  -- | custom puzzle input
  String ->
  IO ()
execSolutionWith_ ri = void . execSolutionWith ri

-- | Result-suppressing version of 'testSolution'.
testSolution_ :: RunInteractive -> IO ()
testSolution_ = void . testSolution

-- | Result-suppressing version of 'testSolutionOnly'.
testSolutionOnly_ :: RunInteractive -> IO ()
testSolutionOnly_ = void . testSolution

-- | Result-suppressing version of 'viewPrompt'.
viewPrompt_ :: RunInteractive -> IO ()
viewPrompt_ = void . viewPrompt

-- | Result-suppressing version of 'waitForPrompt'.
waitForPrompt_ :: RunInteractive -> IO ()
waitForPrompt_ = void . waitForPrompt

-- | Result-suppressing version of 'submitSolution'.
submitSolution_ :: RunInteractive -> IO ()
submitSolution_ = void . submitSolution

-- | Run the parser of a solution, given its 'ChallengeSpec'.
--
-- @
-- 'loadParseInput' (solSpec 'day01a) day01a
-- @
loadParseInput :: Integer -> ChallengeSpec -> a :~> b -> IO a
loadParseInput yr cs s = eitherIO $ do
  i <- liftIO $ loadInput yr cs
  maybeToEither ["No parse"] $ sParse s i

-- | Run the parser of a solution on test data, given its 'ChallengeSpec'.
--
-- @
-- 'loadParseTests' (solSpec 'day01a) day01a
-- @
loadParseTests :: Integer -> ChallengeSpec -> a :~> b -> IO [(Maybe a, TestMeta)]
loadParseTests yr cs s = (map . first) (sParse s) <$> loadTests yr cs

-- | Load input for a given challenge
loadInput :: Integer -> ChallengeSpec -> IO String
loadInput yr cs = eitherIO $ do
  CD{..} <- liftIO $ do
    Cfg{..} <- configFile defConfPath
    challengeData _cfgSession yr cs
  liftEither _cdInput

-- | Load test cases for a given challenge
loadTests :: Integer -> ChallengeSpec -> IO [(String, TestMeta)]
loadTests yr cs = do
  Cfg{..} <- configFile defConfPath
  _cdTests <$> challengeData _cfgSession yr cs

-- | Unsafely create a 'ChallengeSpec' from a day number and part.
--
-- Is undefined if given a day number out of range (1-25).
mkSpec :: Integer -> Part -> ChallengeSpec
mkSpec = CS . mkDay_

eitherIO :: ExceptT [String] IO a -> IO a
eitherIO act =
  runExceptT act >>= \case
    Right x -> pure x
    Left es -> fail $ unlines es

-- | As a splice like `$(fromSol 'day21)`, creates a 'RunInteractive' for
-- usage with this module.
--
-- Assumes that most final 4-digit number in the module components represents
-- the year.
fromSol :: TH.Name -> TH.Q TH.Exp
fromSol nm = do
  ss <- TH.unTypeCode $ specSomeSol nm
  pure $
    TH.RecConE
      'RI
      [ ('_riYear, TH.LitE (TH.IntegerL year))
      ,
        ( '_riSpec
        , TH.RecConE
            'CS
            [ ('_csDay, TH.unType $ liftDay d)
            , ('_csPart, TH.unType $ liftPart p)
            ]
        )
      , ('_riSolution, ss)
      ]
  where
    CS d p = solSpec nm
    year :: Integer
    year =
      read
        . fromMaybe (error "Expected 4-digit number in module name chain")
        . listToMaybe
        . reverse
        . filter ((== 4) . length)
        . words
        . map (\c -> if isDigit c then c else ' ')
        . fromMaybe (error "Identifier needs a module")
        $ TH.nameModule nm
