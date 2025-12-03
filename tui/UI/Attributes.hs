{-# LANGUAGE OverloadedStrings #-}

-- | Attribute definitions for the TUI
--
-- This module defines named attributes for styling markdown and UI elements.
-- Brick requires attributes to be named and defined in an attribute map.
module UI.Attributes
  ( -- * Attribute names
    header1Attr
  , header2Attr
  , header3Attr
  , boldAttr
  , italicAttr
  , underlineAttr
  , codeAttr
  , codeBlockAttr
  , linkAttr
  , strikethroughAttr
  , quotedAttr
  , inputPanelAttr
  , inputPanelLabelAttr
  , inputPanelHelpAttr
  , logInfoAttr
  , logWarningAttr
  , logErrorAttr
    -- * Attribute map
  , theMap
  ) where

import Brick.AttrMap (AttrName, attrName, AttrMap, attrMap)
import qualified Graphics.Vty as V

-- | Attribute names for markdown elements
header1Attr, header2Attr, header3Attr :: AttrName
header1Attr = attrName "header1"
header2Attr = attrName "header2"
header3Attr = attrName "header3"

boldAttr, italicAttr, underlineAttr :: AttrName
boldAttr = attrName "bold"
italicAttr = attrName "italic"
underlineAttr = attrName "underline"

codeAttr, codeBlockAttr :: AttrName
codeAttr = attrName "code"
codeBlockAttr = attrName "codeBlock"

linkAttr, strikethroughAttr, quotedAttr :: AttrName
linkAttr = attrName "link"
strikethroughAttr = attrName "strikethrough"
quotedAttr = attrName "quoted"

inputPanelAttr, inputPanelLabelAttr, inputPanelHelpAttr :: AttrName
inputPanelAttr = attrName "inputPanel"
inputPanelLabelAttr = attrName "inputPanelLabel"
inputPanelHelpAttr = attrName "inputPanelHelp"

logInfoAttr, logWarningAttr, logErrorAttr :: AttrName
logInfoAttr = attrName "logInfo"
logWarningAttr = attrName "logWarning"
logErrorAttr = attrName "logError"

-- | The attribute map defining all styles
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (header1Attr, V.defAttr `V.withStyle` V.bold `V.withForeColor` V.cyan)
  , (header2Attr, V.defAttr `V.withStyle` V.bold `V.withForeColor` V.blue)
  , (header3Attr, V.defAttr `V.withStyle` V.bold)
  , (boldAttr, V.defAttr `V.withStyle` V.bold)
  , (italicAttr, V.defAttr `V.withStyle` V.italic)
  , (underlineAttr, V.defAttr `V.withStyle` V.underline)
  , (codeAttr, V.defAttr `V.withForeColor` V.yellow)
  , (codeBlockAttr, V.defAttr `V.withBackColor` V.black `V.withForeColor` V.white)
  , (linkAttr, V.defAttr `V.withForeColor` V.cyan `V.withStyle` V.underline)
  , (strikethroughAttr, V.defAttr `V.withStyle` V.strikethrough)
  , (quotedAttr, V.defAttr `V.withForeColor` V.green)
  -- Input panel styling - bright colors to make it stand out
  , (inputPanelAttr, V.defAttr `V.withForeColor` V.brightWhite `V.withBackColor` V.black)
  , (inputPanelLabelAttr, V.defAttr `V.withStyle` V.bold `V.withForeColor` V.brightYellow `V.withBackColor` V.black)
  , (inputPanelHelpAttr, V.defAttr `V.withStyle` V.italic `V.withForeColor` V.brightCyan `V.withBackColor` V.black)
  -- Log level styling - subtle markers
  , (logInfoAttr, V.defAttr `V.withForeColor` V.cyan)
  , (logWarningAttr, V.defAttr `V.withForeColor` V.yellow)
  , (logErrorAttr, V.defAttr `V.withForeColor` V.red)
  ]
