{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module UI.UserInput
  ( -- * Effect
    UserInput (..)
  , requestInput

    -- * Re-exports for convenience
  , module UI.UserInput.InputWidget
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Polysemy
import UI.UserInput.InputWidget

-- | Universal user input effect - parameterized by widget system
-- The 'widget' parameter allows different UI implementations (TUI, GUI, CLI, etc.)
-- to provide their own widget rendering while keeping the effect universal.
data UserInput widget (m :: Type -> Type) a where
  RequestInput :: ImplementsWidget widget a => Text -> a -> UserInput widget m a
  -- ^ RequestInput prompt defaultValue
  -- Prompts the user for input, showing the prompt text and using
  -- the default value as both a hint and the initial value.
  -- Returns a value of the same type as the default.
  -- The ImplementsWidget constraint ensures the widget system can handle type 'a'.

-- Manual definition to avoid ambiguous type variable
requestInput :: forall widget r a. (Member (UserInput widget) r, ImplementsWidget widget a)
             => Text -> a -> Sem r a
requestInput prompt defaultValue = Polysemy.send (RequestInput @widget prompt defaultValue :: UserInput widget (Sem r) a)
