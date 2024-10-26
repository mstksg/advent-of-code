module AOC.Main.Bench (benchFor) where

import AOC.Run
import AOC.Run.Config
import Control.Monad
import Control.Monad.Except

benchFor :: ChallengeBundle -> IO ()
benchFor cb = do
  cfg <- configFile defConfPath
  void . runExceptT . mainRun cb cfg $
    (defaultMRO TSAll)
      { _mroBench = True
      }
