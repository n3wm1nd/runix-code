{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Streaming support for the TUI
--
-- Cancellation support for the TUI. SSE chunk processing has moved to
-- 'UI.UserInterface.interpretAsWidget' which handles it per-widget.
module UI.Streaming
  ( interpretCancellation
  ) where

import Control.Concurrent.STM
import Polysemy (Member, Sem, Embed, embed, interpret)
import Runix.Cancellation (Cancellation(..))
import UI.State (UIVars, readCancellationFlag)

--------------------------------------------------------------------------------
-- Cancellation
--------------------------------------------------------------------------------

-- | Interpret Cancellation effect for TUI
--
-- Reads the cancellation flag from UIVars (set by UI when user presses ESC).
-- The flag is checked at strategic points: before QueryLLM, between HTTP chunks.
interpretCancellation :: Member (Embed IO) r
                      => UIVars msg
                      -> Sem (Cancellation : r) a
                      -> Sem r a
interpretCancellation uiVars = interpret $ \case
  IsCanceled ->
    -- Check the STM flag atomically
    embed $ atomically $ readCancellationFlag uiVars
