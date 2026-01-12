{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Serialization format for editable messages only
--
-- Simple text format for USER/ASSISTANT messages
-- Used by /history command to allow editing in $EDITOR
module UI.MessageFormat
  ( formatEditableMessages
  , parseEditableMessages
  , isEditableMessage
  , ParseError(..)
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import UniversalLLM (Message(..))

--------------------------------------------------------------------------------
-- Filtering
--------------------------------------------------------------------------------

-- | Check if a message is editable (can be modified in editor)
-- Currently only USER and ASSISTANT text messages
isEditableMessage :: Message model -> Bool
isEditableMessage (UserText _) = True
isEditableMessage (AssistantText _) = True
isEditableMessage _ = False

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

-- | Format editable messages for editing in $EDITOR
-- Only serializes USER and ASSISTANT messages
formatEditableMessages :: [Message model] -> Text
formatEditableMessages msgs = T.concat $ map formatMessage $ filter isEditableMessage msgs

-- | Format a single editable message
formatMessage :: Message model -> Text
formatMessage (UserText text) =
  "=== USER ===\n" <> text <> "\n"
formatMessage (AssistantText text) =
  "=== ASSISTANT ===\n" <> text <> "\n"
formatMessage _ = ""  -- Should never happen (filtered out)

--------------------------------------------------------------------------------
-- Deserialization
--------------------------------------------------------------------------------

-- | Parse errors
data ParseError
  = UnknownMessageType Text
  | MissingContent Text
  | InvalidFormat Text
  deriving stock (Eq, Show)

-- | Parse editable messages from formatted text
parseEditableMessages :: Text -> Either ParseError [Message model]
parseEditableMessages input =
  let sections = splitSections input
  in mapM parseSection sections

-- | Split input into sections based on === HEADER === markers
splitSections :: Text -> [(Text, Text)]
splitSections input = go (T.lines input) Nothing []
  where
    go :: [Text] -> Maybe Text -> [Text] -> [(Text, Text)]
    go [] Nothing _ = []
    go [] (Just header) contentLines =
      [(header, T.unlines (reverse contentLines))]
    go (line:rest) currentHeader contentLines
      | Just header <- extractHeader line =
          case currentHeader of
            Nothing ->
              go rest (Just header) []
            Just prevHeader ->
              (prevHeader, T.unlines (reverse contentLines)) : go rest (Just header) []
      | otherwise =
          go rest currentHeader (line:contentLines)

    extractHeader :: Text -> Maybe Text
    extractHeader line = do
      stripped <- T.stripPrefix "===" line
      header <- T.stripSuffix "===" stripped
      return (T.strip header)

-- | Parse a single section
parseSection :: (Text, Text) -> Either ParseError (Message model)
parseSection (header, content) =
  let cleanContent = T.strip content
  in case header of
    "USER" ->
      if T.null cleanContent
      then Left (MissingContent "USER message has empty content")
      else Right (UserText cleanContent)

    "ASSISTANT" ->
      if T.null cleanContent
      then Left (MissingContent "ASSISTANT message has empty content")
      else Right (AssistantText cleanContent)

    _ ->
      Left (UnknownMessageType header)
