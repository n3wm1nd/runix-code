{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

-- | Interpreter for ForegroundCmd effect
module UI.ForegroundCmdInterpreter where

import Polysemy
import UI.ForegroundCmd
import UI.State (UIVars, sendAgentEvent, AgentEvent(..))
import Runix.Logging (Level(..))
import System.Process (callProcess, waitForProcess, spawnProcess)
import System.IO.Error (catchIOError)
import qualified Data.Text as T

-- | Interpret ForegroundCmd by sending events to the UI thread
interpretForegroundCmd :: Member (Embed IO) r
                       => UIVars msg
                       -> Sem (ForegroundCmd ': r) a
                       -> Sem r a
interpretForegroundCmd uiVars = interpret $ \case
  RunForegroundCmd cmd args -> embed $ sendAgentEvent uiVars (RunExternalCommandEvent (runSafely cmd args))
    where
      runSafely :: String -> [String] -> IO ()
      runSafely command arguments =
        -- Catch IO errors (file not found, permission denied, etc.) but ignore exit codes
        catchIOError
          (do ph <- spawnProcess command arguments
              _ <- waitForProcess ph  -- Wait but ignore exit code
              return ())
          (\e -> sendAgentEvent uiVars (LogEvent Error (T.pack $ "Failed to run command: " ++ show e)))
