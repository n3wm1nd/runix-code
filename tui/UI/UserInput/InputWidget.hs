{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.UserInput.InputWidget
  ( InputWidget (..)
  , ImplementsWidget (..)
  , RenderRequest (..)
  , TUIWidget
  ) where

import Brick
  ( AttrName
  , EventM
  , Widget
  , attrName
  , hBox
  , txt
  , vBox
  , withAttr
  , (<+>)
  )
import Brick.Types (BrickEvent (..))
import Data.Char (digitToInt, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty
  ( Event (..)
  , Key (..)
  )

-- Re-export universal interface from library
import UI.UserInput (ImplementsWidget (..), RenderRequest)

-- | TUI-specific RenderRequest constructor
-- The constraint dict brings the necessary typeclass evidence
data instance RenderRequest TUIWidget a where
  RenderRequest :: InputWidget a
                => Text -> a -> RenderRequest TUIWidget a

-- | Phantom type tag for the TUI (Brick-based) widget system
data TUIWidget

-- | Types that can be requested from the user via the UserInput effect.
-- The interpreter uses this typeclass to determine how to present and
-- handle input for different types.
class InputWidget a where
  -- | Render the input widget in the UI
  renderWidget :: Text -> a -> Widget n

  -- | Handle a user event, potentially updating the value
  -- Returns Just newValue if the event changed the value, Nothing otherwise
  handleWidgetEvent :: a -> BrickEvent n e -> EventM n () (Maybe a)

  -- | Check if the current value is valid/complete (enables Submit)
  isWidgetComplete :: a -> Bool

  -- Default implementations that fail at runtime
  default renderWidget :: Show a => Text -> a -> Widget n
  renderWidget prompt val =
    vBox
      [ txt prompt
      , txt ""
      , txt "⚠ No InputWidget instance for this type!"
      , txt $ "Current value: " <> T.pack (show val)
      , txt "(Cannot edit - implement InputWidget instance)"
      ]

  default handleWidgetEvent :: a -> BrickEvent n e -> EventM n () (Maybe a)
  handleWidgetEvent _ _ = return Nothing

  default isWidgetComplete :: a -> Bool
  isWidgetComplete _ = False

-- | Bool: Yes/No confirmation dialog
instance InputWidget Bool where
  renderWidget prompt current =
    vBox
      [ txt prompt
      , txt ""
      , hBox
          [ if current
              then withAttr selectedAttr (txt "[Yes]")
              else txt "Yes"
          , txt "  "
          , if not current
              then withAttr selectedAttr (txt "[No]")
              else txt "No"
          ]
      , txt ""
      , txt "← → / y/n: Toggle | Enter: Confirm"
      ]

  handleWidgetEvent current ev = case ev of
    VtyEvent (EvKey (KChar 'y') []) -> return $ Just True
    VtyEvent (EvKey (KChar 'n') []) -> return $ Just False
    VtyEvent (EvKey KLeft []) -> return $ Just (not current)
    VtyEvent (EvKey KRight []) -> return $ Just (not current)
    _ -> return Nothing

  isWidgetComplete = const True

-- | Text: Freeform text input
instance InputWidget Text where
  renderWidget prompt current =
    vBox
      [ txt prompt
      , txt ""
      , txt "> " <+> txt current <+> txt "▊"
      , txt ""
      , txt "Type to edit | Backspace: Delete | Enter: Confirm"
      ]

  handleWidgetEvent current ev = case ev of
    VtyEvent (EvKey (KChar c) []) -> return $ Just (current <> T.singleton c)
    VtyEvent (EvKey KBS []) ->
      return $ Just (if T.null current then current else T.init current)
    VtyEvent (EvKey KDel []) ->
      return $ Just (if T.null current then current else T.init current)
    _ -> return Nothing

  isWidgetComplete = const True  -- Allow empty responses

-- | Int: Numeric input with increment/decrement
instance InputWidget Int where
  renderWidget prompt current =
    vBox
      [ txt prompt
      , txt ""
      , txt "> " <+> txt (T.pack $ show current) <+> txt "▊"
      , txt ""
      , txt "0-9: Type digit | Backspace: Delete | ↑↓: Inc/Dec | Enter: Confirm"
      ]

  handleWidgetEvent current ev = case ev of
    VtyEvent (EvKey (KChar c) []) | isDigit c ->
      return $ Just (current * 10 + digitToInt c)
    VtyEvent (EvKey KBS []) -> return $ Just (current `div` 10)
    VtyEvent (EvKey KUp []) -> return $ Just (current + 1)
    VtyEvent (EvKey KDown []) -> return $ Just (current - 1)
    VtyEvent (EvKey (KChar '-') []) -> return $ Just (negate current)
    _ -> return Nothing

  isWidgetComplete = const True

-- | Maybe a: Optional value - can be None or Just a
instance {-# OVERLAPPABLE #-} InputWidget a => InputWidget (Maybe a) where
  renderWidget prompt current =
    vBox $
      [ txt prompt
      , txt ""
      , txt "[Tab: Toggle None | Esc: Set to None]"
      , txt ""
      ]
        ++ case current of
          Nothing -> [withAttr dimmedAttr $ txt "(None - will use default)"]
          Just val -> [renderWidget @a "" val]

  handleWidgetEvent current ev = case ev of
    VtyEvent (EvKey (KChar '\t') []) ->
      -- Toggle between Nothing and Just
      return $ Just $ case current of
        Nothing -> Just undefined -- Will be replaced with proper default
        Just _ -> Nothing
    VtyEvent (EvKey KEsc []) -> return $ Just Nothing
    _ -> case current of
      Just val -> do
        mNewVal <- handleWidgetEvent val ev
        return $ fmap Just mNewVal
      Nothing -> return Nothing

  isWidgetComplete = const True -- Nothing is valid

-- | List [a]: Multiple values (not implemented yet - placeholder)
instance {-# OVERLAPPABLE #-} (Show a, InputWidget a) => InputWidget [a] where
  renderWidget prompt currentList =
    vBox
      [ txt prompt
      , txt ""
      , txt "List editing not yet implemented"
      , txt $ "Current: " <> T.pack (show currentList)
      ]

  handleWidgetEvent _ _ = return Nothing

  isWidgetComplete = const True

-- Attributes used for styling
selectedAttr :: AttrName
selectedAttr = attrName "selected"

dimmedAttr :: AttrName
dimmedAttr = attrName "dimmed"

--------------------------------------------------------------------------------
-- Bridge Instances: Connect InputWidget to ImplementsWidget
--------------------------------------------------------------------------------

-- | If a type has an InputWidget instance, it implements the TUIWidget interface
-- The implementation simply creates a RenderRequest - the interpreter handles the rest
instance {-# OVERLAPPABLE #-} InputWidget a => ImplementsWidget TUIWidget a where
  askWidget prompt defaultValue = RenderRequest prompt defaultValue
