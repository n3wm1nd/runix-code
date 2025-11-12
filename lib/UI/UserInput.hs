{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module UI.UserInput
  ( -- * Effect
    UserInput (..)
  , requestInput

    -- * Universal Interface
  , ImplementsWidget (..)
  , RenderRequest (..)

    -- * Simple Interpreters
  , interpretUserInputFail
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Polysemy
import Polysemy.Fail

-- | Universal widget interface bridge
-- This class connects the universal UserInput effect to specific UI implementations.
-- The 'widget' parameter is a phantom type that tags which UI system we're using.
-- Instead of providing implementation, it creates a request that the interpreter fulfills.
class ImplementsWidget widget a where
  askWidget :: Text -> a -> RenderRequest widget a

-- | A request for the widget system to render and get input
-- Each widget system provides its own GADT constructor with appropriate constraints
-- This is a data family so each UI system can define its own instance
data family RenderRequest widget a

-- | Universal user input effect - parameterized by widget system
-- The 'widget' parameter allows different UI implementations (TUI, GUI, CLI, etc.)
-- to provide their own widget rendering while keeping the effect universal.
data UserInput widget (m :: Type -> Type) a where
  RequestInput :: ImplementsWidget widget a => Text -> a -> UserInput widget m (Maybe a)
  -- ^ RequestInput prompt defaultValue
  -- Prompts the user for input, showing the prompt text and using
  -- the default value as both a hint and the initial value.
  -- Returns Maybe a: Just value if user confirmed, Nothing if user cancelled.
  -- The ImplementsWidget constraint ensures the widget system can handle type 'a'.

-- Manual definition to avoid ambiguous type variable
requestInput :: forall widget r a. (Member (UserInput widget) r, ImplementsWidget widget a)
             => Text -> a -> Sem r (Maybe a)
requestInput prompt defaultValue = Polysemy.send (RequestInput @widget prompt defaultValue :: UserInput widget (Sem r) (Maybe a))

-- | Simple interpreter that always fails for non-interactive clients
-- This is useful for CLI tools or other non-interactive environments where
-- user input is not supported. Any attempt to request input will fail.
interpretUserInputFail :: forall widget r a. Member Fail r
                       => Sem (UserInput widget ': r) a
                       -> Sem r a
interpretUserInputFail = interpret $ \case
  RequestInput prompt _ -> fail $ "User input not supported: " <> show prompt
