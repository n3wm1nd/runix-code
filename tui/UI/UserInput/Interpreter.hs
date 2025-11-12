{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.UserInput.Interpreter
  ( interpretUserInput
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Polysemy
import Polysemy.Embed (Embed)
import UI.State (SomeInputWidget (..), UIVars, setPendingInput, clearPendingInput)
import UI.UserInput (UserInput (..))
import UI.UserInput.InputWidget (InputWidget, TUIWidget, ImplementsWidget (..), RenderRequest (..))

-- | Interpret UserInput effect using the TUI (Brick) widget system
-- This interpreter:
-- 1. Creates a response variable for this specific request
-- 2. Packages the widget with the callback into SomeInputWidget
-- 3. Sets it in UIState (which triggers UI to show the widget)
-- 4. Blocks until the callback is invoked by the UI
interpretUserInput
  :: forall r a
   . Member (Embed IO) r
  => UIVars
  -> Sem (UserInput TUIWidget ': r) a
  -> Sem r a
interpretUserInput uiVars = interpret $ \case
  RequestInput prompt (defaultValue :: x) -> embed @IO $ do
    -- Get the render request from ImplementsWidget
    let request = askWidget prompt defaultValue :: RenderRequest TUIWidget x

    -- Fulfill the request using TUI-specific logic
    fulfillRequest uiVars request

-- | Fulfill a TUI render request
-- This is where the TUI-specific implementation lives
-- The InputWidget constraint comes from pattern matching on the GADT
fulfillRequest :: forall a. UIVars -> RenderRequest TUIWidget a -> IO a
fulfillRequest uiVars (RenderRequest prompt defaultValue) = do
  -- Pattern matching brings InputWidget a constraint
  -- Create a response variable for this specific request
  responseVar <- newTVarIO (Nothing :: Maybe a)

  -- Create callback that UI will invoke when user submits
  let submitCallback :: a -> IO ()
      submitCallback value = atomically $ writeTVar responseVar (Just value)

  -- Package everything into existential wrapper
  let widget = SomeInputWidget prompt defaultValue submitCallback

  -- Set pending input in UI state (triggers UI to show widget)
  setPendingInput uiVars widget

  -- Block until user provides input
  result <- atomically $ waitForResponse responseVar

  -- Clear the widget from UI
  clearPendingInput uiVars

  return result

-- | Wait for response (blocks until Just value appears)
waitForResponse :: TVar (Maybe a) -> STM a
waitForResponse var = do
  mValue <- readTVar var
  case mValue of
    Nothing -> retry
    Just value -> return value
