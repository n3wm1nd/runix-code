{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Bottom panel for input widgets
module UI.InputPanel
  ( drawInputPanel
  , handleInputPanelEvent
  ) where

import Brick
  ( EventM
  , Padding(..)
  , Widget
  , padAll
  , padLeft
  , padRight
  , str
  , txt
  , vBox
  , vLimit
  , withAttr
  , withBorderStyle
  
  )
import Brick.Types (BrickEvent)
import Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.Border.Style as BS
import UI.Attributes (inputPanelAttr, inputPanelLabelAttr, inputPanelHelpAttr)
import UI.State (Name)
import UI.State (SomeInputWidget (..))
import UI.UserInput.InputWidget (InputWidget(renderWidget, handleWidgetEvent))

-- | Draw the input panel for an active input widget
-- Returns the widget UI and its height
drawInputPanel :: SomeInputWidget -> (Widget Name, Int)
drawInputPanel (SomeInputWidget prompt currentValue _submitCallback) =
  let panelHeight = 10
      label = withAttr inputPanelLabelAttr $ txt " Agent requesting input "
      panel = withAttr inputPanelAttr $
        vLimit panelHeight $
          withBorderStyle BS.unicodeBold $
            borderWithLabel label $
              padLeft Max $ padRight Max $ padAll 1 $ vBox
                [ renderWidget prompt currentValue
                , str ""
                , withAttr inputPanelHelpAttr $ txt "⏎ Confirm | Esc Cancel"
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
