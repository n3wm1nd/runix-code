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
import Polysemy
import UI.State (UIVars, sendAgentEvent, SomeInputWidget (..), AgentEvent(..))
import UI.UserInput (UserInput (..))
import UI.UserInput.InputWidget (TUIWidget, ImplementsWidget (..), RenderRequest (..))

-- | Interpret UserInput effect using the TUI (Brick) widget system
-- This interpreter:
-- 1. Creates a response variable for this specific request
-- 2. Packages the widget with the callback into SomeInputWidget
-- 3. Sets it in UIState (which triggers UI to show the widget)
-- 4. Blocks until the callback is invoked by the UI
-- Returns Maybe a: Just = confirmed, Nothing = cancelled
interpretUserInput
  :: forall msg r a
   . Member (Embed IO) r
  => UIVars msg
  -> Sem (UserInput TUIWidget ': r) a
  -> Sem r a
interpretUserInput uiVars = interpret $ \case
  RequestInput prompt (defaultValue :: x) -> do
    -- Get the render request from ImplementsWidget
    let request = askWidget prompt defaultValue :: RenderRequest TUIWidget x

    -- Fulfill the request using TUI-specific logic
    fulfillRequest uiVars request

-- | Fulfill a TUI render request
-- This is where the TUI-specific implementation lives
-- The InputWidget constraint comes from pattern matching on the GADT
fulfillRequest :: forall msg a r. Member (Embed IO) r => UIVars msg -> RenderRequest TUIWidget a -> Sem r (Maybe a)
fulfillRequest uiVars (RenderRequest prompt defaultValue) = do
  -- Pattern matching brings InputWidget a constraint
  -- Create a response variable for this specific request
  -- Outer Maybe: Nothing = waiting, Just = user responded
  -- Inner Maybe: Nothing = cancelled, Just = confirmed with value
  responseVar <- embed $ newTVarIO (Nothing :: Maybe (Maybe a))

  -- Create callback that UI will invoke when user submits
  let submitCallback :: Maybe a -> IO ()
      submitCallback mValue = atomically $ writeTVar responseVar (Just mValue)

  -- Package everything into existential wrapper
  let widget = SomeInputWidget prompt defaultValue submitCallback

  -- Send event to show the widget (triggers UI to display it)
  embed $ sendAgentEvent uiVars (ShowInputWidgetEvent widget)

  -- Block until user provides input
  result <- embed $ atomically $ waitForResponse responseVar

  -- Send event to clear the widget from UI
  embed $ sendAgentEvent uiVars ClearInputWidgetEvent

  -- Return the Maybe a directly (Nothing = cancelled, Just = confirmed)
  return result

-- | Wait for response (blocks until Just value appears)
waitForResponse :: TVar (Maybe (Maybe a)) -> STM (Maybe a)
waitForResponse var = do
  mValue <- readTVar var
  case mValue of
    Nothing -> retry
    Just result -> return result
