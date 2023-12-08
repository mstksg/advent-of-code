{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : AOC.Interactive
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Run actions regarding challenges, solutions, tests, submissions, viewing
-- prompts, etc.
--
-- Essentially implements the functionality of the main app.
--

module AOC.Run (
  -- * Options
    TestSpec(..)
  -- * Runners
  -- ** Run solutions, tests, benchmarks
  , MainRunOpts(..), HasMainRunOpts(..), mainRun, defaultMRO
  -- ** View prompts
  , MainViewOpts(..), HasMainViewOpts(..), mainView, defaultMVO
  -- ** Submit answers
  , MainSubmitOpts(..), HasMainSubmitOpts(..), mainSubmit, defaultMSO
  -- * Util
  , withColor
  ) where

import           AOC.Discover
import           AOC.Run.Config
import           AOC.Run.Load
import           AOC.Solver
import           AOC.Util
import           Advent
import           Control.Applicative
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Criterion
import           Data.Bifunctor
import           Data.Char
import           Data.Map                 (Map)
import           Data.Maybe
import           Data.Text                (Text)
import           Data.Time hiding         (Day)
import           Text.Printf
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified System.Console.ANSI      as ANSI
import qualified System.Console.Haskeline as H

type ChallengeSet = Map ChallengeSpec

-- | Specification of parts to test and run
data TestSpec = TSAll
              | TSYear Integer
              | TSDay Integer Day
              | TSPart ChallengeSpec
  deriving stock Show

-- | Options for 'mainRun'.
data MainRunOpts = MRO { _mroSpec   :: !TestSpec
                       , _mroActual :: !Bool     -- ^ Run input?  (Defualt: True
                       , _mroTest   :: !Bool     -- ^ Run tests?  (Default: False)
                       , _mroBench  :: !Bool     -- ^ Benchmark?  (Default: False)
                       , _mroLock   :: !Bool     -- ^ Lock in answer as correct?  (Default: False)
                       , _mroInput  :: !(ChallengeSpec -> IO (Maybe String))   -- ^ Manually supply input (Default: always return Nothing)
                       }

makeClassy ''MainRunOpts

-- | Options for 'mainView'.
data MainViewOpts = MVO { _mvoSpec :: !TestSpec
                        , _mvoWait :: !Bool
                        }
  deriving stock Show

makeClassy ''MainViewOpts

-- | Options for 'mainSubmit'
data MainSubmitOpts = MSO { _msoTest  :: !Bool    -- ^ Run tests before submitting?  (Default: True)
                          , _msoForce :: !Bool    -- ^ Force submission even if bad?  (Default: False)
                          , _msoLock  :: !Bool    -- ^ Lock answer if submission succeeded?  (Default: True)
                          }
  deriving stock Show

makeClassy ''MainSubmitOpts

-- | Default options for 'mainRun'.
defaultMRO :: TestSpec -> MainRunOpts
defaultMRO ts = MRO { _mroSpec   = ts
                    , _mroActual = True
                    , _mroTest   = False
                    , _mroBench  = False
                    , _mroLock   = False
                    , _mroInput  = \_ -> pure Nothing
                    }

-- | Default options for 'mainView'.
defaultMVO :: TestSpec -> MainViewOpts
defaultMVO ts = MVO { _mvoSpec = ts
                    , _mvoWait = False
                    }

-- | Default options for 'mainSubmit'.
defaultMSO :: MainSubmitOpts
defaultMSO = MSO { _msoTest  = True
                 , _msoForce = False
                 , _msoLock  = True
                 }


toChallengeSet :: ChallengeMap -> TestSpec -> Either String (ChallengeSet SomeSolution)
toChallengeSet challengeMap = \case
    TSAll    -> pure $ M.fromList
        [ (CS y d p, c)
        | (y, ds) <- M.toList challengeMap
        , (d, ps) <- M.toList ds
        , (p, c) <- M.toList ps
        ]
    TSYear y -> do
      ds <- maybeToEither (printf "Year not yet avaiable: %04d" y) $
        M.lookup y challengeMap
      pure $ M.fromList
        [ (CS y d p, c)
        | (d, ps) <- M.toList ds
        , (p, c) <- M.toList ps
        ]
    TSDay y d -> do
      ds <- maybeToEither (printf "Year not yet avaiable: %04d" y) $
                     M.lookup y challengeMap
      ps <- maybeToEither (printf "Day not yet available: %d" (dayInt d)) $
              M.lookup d ds
      pure $ M.fromList
        [ (CS y d p, c)
        | (p, c) <- M.toList ps
        ]
    TSPart cs@(CS y d p) -> do
      ds <- maybeToEither (printf "Year not yet avaiable: %04d" y) $
                     M.lookup y challengeMap
      ps <- maybeToEither (printf "Day not yet available: %d" (dayInt d)) $
              M.lookup d ds
      c  <- maybeToEither (printf "Part not found: %c" (partChar p)) $
              M.lookup p ps
      pure $ M.singleton cs c

-- | Run, test, bench.
mainRun
    :: (MonadIO m, MonadError [String] m)
    => ChallengeMap
    -> Config
    -> MainRunOpts
    -> m (ChallengeSet (Maybe Bool, Either [String] String))  -- whether or not passed tests, and result
mainRun challengeMap Cfg{..} MRO{..} =  do
    toRun <- liftEither . first (:[]) . toChallengeSet challengeMap $ _mroSpec
    liftIO . runAll _cfgSession _mroLock _mroInput toRun $ \c inp0 cd@CD{..} -> do
      testRes <- fmap join . forM (guard _mroTest) $ \_ ->
        runTestSuite c cd

      let inp1 = maybe _cdInput  Right           inp0
          ans1 = maybe _cdAnswer (const Nothing) inp0
      case inp1 of
        Right inp
          | _mroBench -> do
              _ <- evaluate (force inp)
              let res    = (testRes, Left ["Ran benchmark, so no result"])
              res <$ case c of
                MkSomeSolWH _         ->
                      benchmark (nf (runSomeSolution c) inp)
                MkSomeSolNF MkSol{..}
                  | Just x <- sParse inp -> do
                      _ <- evaluate (force x)
                      benchmark (nf (let ?dyno = mempty in sSolve) x)
                      putStrLn "* parsing and formatting times excluded"
                      putStrLn ""
                  | otherwise            ->
                      putStrLn "(No parse)"
          | _mroActual -> (second . first) ((:[]) . show) <$> testCase False c inp (TM ans1 M.empty)
          | otherwise   -> pure (testRes, Left ["Actual input skipped"])
        Left e
          | _mroTest  -> pure (testRes, Left ["Ran tests, so no result"])
          | otherwise -> (testRes, Left e) <$ putStrLn "[INPUT ERROR]" <* mapM_ putStrLn e

-- | View prompt
mainView
    :: (MonadIO m, MonadError [String] m)
    => ChallengeMap
    -> Config
    -> MainViewOpts
    -> m (Map ChallengeSpec Text)
mainView challengeMap Cfg{..} MVO{..} = do
    let toRun :: ChallengeSet ()
        toRun = maybe M.empty void
              . eitherToMaybe
              . toChallengeSet challengeMap
              $ _mvoSpec
    flip M.traverseWithKey toRun $ \cs@CS{..} _ -> do
      pmpt   <- waitFunc _csDay $ do
        CD{..} <- liftIO $ challengeData _cfgSession cs
        liftEither . first ("[PROMPT ERROR]":) $ _cdPrompt
      liftIO $ do
        withColor ANSI.Dull ANSI.Blue $
          printf ">> %04d Day %02d%c\n" _csYear (dayInt _csDay) (partChar _csPart)
        T.putStrLn pmpt
        putStrLn ""
      pure pmpt
  where
    waitFunc d
      | _mvoWait  = countdownConsole _cfgYear d . (liftIO (threadDelay 500000) *>)
      | otherwise = id
    -- singleTest = case _mvoSpec of
    --   TSAll        -> Nothing
    --   TSDayAll d   -> Just (d, Part1)
    --   TSDayPart cs -> Just (_csDay cs, _csPart cs)

-- | Submit and analyze result
mainSubmit
    :: (MonadIO m, MonadError [String] m)
    => ChallengeMap
    -> Config
    -> ChallengeSpec
    -> MainSubmitOpts
    -> m (Text, SubmitRes)
mainSubmit challengeMap cfg cs@CS{..} mso@MSO{..} = do
    yMap      <- maybeToEither [printf "Year not yet available: %04d" _csYear] $
                   M.lookup _csYear challengeMap
    dMap      <- maybeToEither [printf "Day not yet available: %d" d'] $
                   M.lookup _csDay yMap
    sol       <- maybeToEither [printf "Part not found: %c" (partChar _csPart)] $
                   M.lookup _csPart dMap
    mainSubmit_ sol cfg cs mso
  where
    d' = dayInt _csDay

-- | Submit and analyze result
mainSubmit_
    :: (MonadIO m, MonadError [String] m)
    => SomeSolution
    -> Config
    -> ChallengeSpec
    -> MainSubmitOpts
    -> m (Text, SubmitRes)
mainSubmit_ sol Cfg{..} cs@CS{..} MSO{..} = do
    cd@CD{..} <- liftIO $ challengeData _cfgSession cs
    inp       <- liftEither . first ("[PROMPT ERROR]":) $ _cdInput
    opts      <- defaultAoCOpts _cfgYear <$>
                    maybeToEither ["ERROR: Session Key Required to Submit"]
                      _cfgSession

    when _msoTest $ do
      testRes <- liftIO $ runTestSuite sol cd
      unless (and testRes) $
        if _msoForce
          then liftIO $ putStrLn "Proceeding with submission despite test failures (--force)"
          else do
            conf <- liftIO . H.runInputT H.defaultSettings $
              H.getInputChar "Some tests failed. Are you sure you wish to proceed? y/(n) "
            case toLower <$> conf of
              Just 'y' -> pure ()
              _        -> throwError ["Submission aborted."]

    resEither <- liftIO . evaluate . force . runSomeSolution sol $ inp
    res       <- liftEither . first (("[SOLUTION ERROR]":) . (:[]) . show) $ resEither
    liftIO $ printf "Submitting solution: %s\n" res

    output@(resp, status) <- liftEither . first showAoCError
                         =<< liftIO (runAoC opts (AoCSubmit _csDay _csPart res))
    let resp' = formatResp
              . either (map T.pack) T.lines
              . htmlToMarkdown False
              $ resp
        (color, lock, out) = displayStatus status
    liftIO $ do
      withColor ANSI.Vivid color $
        putStrLn out
      putStrLn resp'
      when lock $
        if _msoLock
          then putStrLn "Locking correct answer." >> writeFile _cpAnswer res
          else putStrLn "Not locking correct answer (--no-lock)"
      zt <- getZonedTime
      appendFile _cpLog $ printf logFmt (show zt) res (showSubmitRes status) resp resp'
    pure output
  where
    CP{..} = challengePaths cs
    formatResp = T.unpack . T.intercalate "\n" . map ("> " <>)
    logFmt = unlines [ "[%s]"
                     , "Submission: %s"
                     , "Status: %s"
                     , "Raw: %s"
                     , "%s"
                     ]

displayStatus :: SubmitRes -> (ANSI.Color, Bool, String)
displayStatus = \case
    SubCorrect r     -> ( ANSI.Green  , True , correctMsg r     )
    SubIncorrect t h -> ( ANSI.Red    , False, incorrectMsg t h )
    SubWait t        -> let (m, s) = t `divMod` 60
                            resp   = printf "Answer re-submitted too soon.  Please wait %dmin %dsec" m s
                        in  ( ANSI.Yellow, False, resp )
    SubInvalid{}     -> ( ANSI.Blue   , False
                        , "Submission was rejected.  Maybe not unlocked yet, or already answered?"
                        )
    SubUnknown{}     -> ( ANSI.Magenta, False
                        , "Response from server was not recognized."
                        )
  where
    correctMsg Nothing  = "Answer was correct!"
    correctMsg (Just r) =
        printf "Answer was correct, and you made the global leaderboard at rank %d !!"
          r
    incorrectMsg t h =
        printf "Answer was incorrect!%s  Please wait %d before submitting again"
          hintStr
          (t `div` 60)
      where
        hintStr :: String
        hintStr = case h of
          Nothing -> ""
          Just s  -> printf "  Hint: Answer was %s." s

runAll
    :: Maybe String                         -- ^ session key
    -> Bool                                 -- ^ run and lock answer
    -> (ChallengeSpec -> IO (Maybe String))   -- ^ replacements
    -> ChallengeSet SomeSolution
    -> (SomeSolution -> Maybe String -> ChallengeData -> IO a)  -- ^ callback. given solution, "replacement" input, and data
    -> IO (ChallengeSet a)
runAll sess lock rep cm f = flip M.traverseWithKey cm $ \cs@(CS yr d p) c -> do
    let CP{..} = challengePaths cs
    inp0 <- rep cs
    withColor ANSI.Dull ANSI.Blue $
      printf ">> Day %02d%c\n" (dayInt d) (partChar p)
    when lock $ do
      CD{..} <- challengeData sess (CS yr d p)
      forM_ (inp0 <|> eitherToMaybe _cdInput) $ \inp ->
        mapM_ (writeFile _cpAnswer) =<< evaluate (force (runSomeSolution c inp))
    f c inp0 =<< challengeData sess (CS yr d p)

runTestSuite :: SomeSolution -> ChallengeData -> IO (Maybe Bool)
runTestSuite c CD{..} = do
    testRes <- mapMaybe fst <$> mapM (uncurry (testCase True c)) _cdTests
    unless (null testRes) $ do
      let (mark, color)
              | and testRes = ('✓', ANSI.Green)
              | otherwise   = ('✗', ANSI.Red  )
      withColor ANSI.Vivid color $
        printf "[%c] Passed %d out of %d test(s)\n"
            mark
            (length (filter id testRes))
            (length testRes)
    pure $ and testRes <$ guard (not (null testRes))


-- | Run a single test case
testCase
    :: Bool             -- ^ is just an example
    -> SomeSolution
    -> String
    -> TestMeta
    -> IO (Maybe Bool, Either SolutionError String)
testCase emph c inp TM{..} = do
    withColor ANSI.Dull color $
      printf "[%c]" mark
    if emph
      then printf " (%s)\n" resStr
      else printf " %s\n"   resStr
    forM_ showAns $ \a ->
      withColor ANSI.Vivid ANSI.Red $
        printf "(Expected: %s)\n" a
    return (status, res)
  where
    res = runSomeSolutionWith _tmData c inp
    resStr = case res of
      Right r -> r
      Left SEParse -> "ERROR: No parse"
      Left SESolve -> "ERROR: No solution"
    (mark, showAns, status) = case _tmAnswer of
      Just (strip->ex)    -> case res of
        Right (strip->r)
          | r == ex   -> ('✓', Nothing, Just True )
          | otherwise -> ('✗', Just ex, Just False)
        Left _        -> ('✗', Just ex, Just False)
      Nothing         -> ('?', Nothing, Nothing   )
    color = case status of
      Just True  -> ANSI.Green
      Just False -> ANSI.Red
      Nothing    -> ANSI.Blue

-- | Do the action with a given ANSI foreground color and intensity.
withColor
    :: ANSI.ColorIntensity
    -> ANSI.Color
    -> IO ()
    -> IO ()
withColor ci c act = do
    ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ci c ]
    act
    ANSI.setSGR [ ANSI.Reset ]

pullMap
    :: Map a (Map b c)
    -> Map (a, b) c
pullMap = M.fromDistinctAscList
        . concatMap (uncurry go . second M.toAscList)
        . M.toAscList
  where
    go x = (map . first) (x,)

pushMap
    :: Eq a
    => Map (a, b) c
    -> Map a (Map b c)
pushMap = fmap M.fromDistinctAscList
        . M.fromAscListWith (flip (++))
        . map (uncurry go)
        . M.toAscList
  where
    go (x, y) z = (x, [(y, z)])
