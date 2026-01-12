{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | ForegroundCmd effect for running commands that need terminal control
--
-- Unlike Runix.Cmd.Cmd which runs in background, this effect
-- suspends the TUI, gives control to the command, then resumes.
module UI.ForegroundCmd where

import Polysemy
import Data.Kind (Type)

-- | Effect for running foreground commands
data ForegroundCmd (m :: Type -> Type) a where
  -- | Run a command in the foreground (suspends TUI, runs command, resumes TUI)
  RunForegroundCmd :: String -> [String] -> ForegroundCmd m ()

makeSem ''ForegroundCmd
