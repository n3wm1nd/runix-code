{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Bottom panel for input widgets
module TUI.InputPanel
  ( drawInputPanel
  , handleInputPanelEvent
  ) where

import Brick
  ( EventM
  , Widget
  , hBox
  , hLimit
  , padAll
  , str
  , txt
  , vBox
  , vLimit
  , (<+>)
  )
import Brick.Types (BrickEvent)
import Brick.Widgets.Border (hBorder)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import UI.State (Name, SomeInputWidget (..))
import UI.UserInput.InputWidget (InputWidget (..))

-- | Draw the input panel for an active input widget
-- Returns the widget UI and its height
drawInputPanel :: SomeInputWidget -> (Widget Name, Int)
drawInputPanel (SomeInputWidget prompt currentValue _submitCallback) =
  let panelHeight = 10
      panel = vLimit panelHeight $
        vBox
          [ hBorder
          , padAll 1 $ vBox
              [ renderWidget prompt currentValue
              , str ""
              , str "Enter: Confirm | Esc: Cancel"
              ]
          ]
   in (panel, panelHeight)

-- | Handle events for the input panel
-- Returns True if event was handled, False otherwise
handleInputPanelEvent
  :: SomeInputWidget
  -> BrickEvent Name e
  -> EventM Name () (Maybe SomeInputWidget)
handleInputPanelEvent widget@(SomeInputWidget prompt currentValue submitCallback) ev = do
  -- Try to handle the event with the widget
  mNewValue <- handleWidgetEvent currentValue ev

  case mNewValue of
    -- Value changed - update the widget
    Just newValue ->
      return $ Just (SomeInputWidget prompt newValue submitCallback)

    -- Event not handled by widget - return original widget unchanged
    Nothing ->
      return $ Just widget
