{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | /history command - Edit conversation history in $EDITOR
module UI.Commands.History
  ( historyCommand
  ) where

import qualified Data.Text.IO as TIO
import Data.Text (Text)
import System.Environment (lookupEnv)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (spawnProcess, waitForProcess)
import Polysemy
import Runix.Logging (Logging, info)
import UniversalLLM (Message(..))
import UI.State (UIVars, sendAgentEvent, AgentEvent(..))
import UI.MessageFormat (formatEditableMessages, parseEditableMessages, isEditableMessage)

-- | /history slash command
historyCommand :: forall model r. Members '[Embed IO, Logging] r
               => [Message model]
               -> UIVars (Message model)
               -> (Text, Text -> Sem r ())
historyCommand history uiVars = ("history", \_ -> do
  -- Edit history in $EDITOR (blocks until editor exits)
  -- Sends AgentCompleteEvent with updated history if successful
  embed $ sendAgentEvent uiVars (RunExternalCommandEvent (editHistoryInEditor history uiVars))

  info "History editing complete")

-- | Edit conversation history in $EDITOR (blocks until editor exits)
-- Sends AgentCompleteEvent with edited messages
editHistoryInEditor :: [Message model] -> UIVars (Message model) -> IO ()
editHistoryInEditor currentHistory uiVars = do
  -- Get editor from environment, default to 'vi'
  maybeEditor <- lookupEnv "EDITOR"
  let editor = maybe "vi" id maybeEditor
      (cmd, cmdArgs) = case words editor of
        [] -> ("vi", [])
        (c:rest) -> (c, rest)

  -- Format only editable messages
  let editableContent = formatEditableMessages currentHistory

  -- Write to temp file and run editor
  withSystemTempFile "runix-code-history.md" $ \path handle -> do
    TIO.hPutStr handle editableContent
    hClose handle

    -- Run editor and wait for it to complete
    ph <- spawnProcess cmd (cmdArgs ++ [path])
    _ <- waitForProcess ph

    -- Read back edited content
    editedContent <- TIO.readFile path

    -- Parse edited messages
    case parseEditableMessages editedContent of
      Left err ->
        -- Parse error: don't update history, just report error
        putStrLn $ "Error parsing edited history: " ++ show err
      Right editedMessages -> do
        -- Merge edited messages back into full history
        -- Strategy: Replace only the editable messages, keep everything else
        let newHistory = mergeEditedHistory editedMessages currentHistory
        -- Send event with updated history to UI
        sendAgentEvent uiVars (AgentCompleteEvent newHistory)

-- | Merge edited messages back into full history
-- Preserves non-editable messages and their relative positions
mergeEditedHistory :: [Message model] -> [Message model] -> [Message model]
mergeEditedHistory editedMessages fullHistory =
  -- Simple approach: walk through full history, replacing editable messages
  -- from editedMessages in order
  go editedMessages fullHistory
  where
    go [] remaining =
      -- No more edited messages: keep only non-editable from remaining
      filter (not . isEditableMessage) remaining

    go edited [] =
      -- No more old messages: add remaining edited
      edited

    go edited@(e:restEdited) (o:restOld)
      | isEditableMessage o =
          if e == o
          then -- Messages match: keep and continue
               e : go restEdited restOld
          else if e `elem` restOld
          then -- Edited message exists later: old was deleted
               go edited restOld
          else if o `elem` restEdited
          then -- Old message exists later: edited is insertion
               e : go restEdited (o:restOld)
          else -- Neither exists later: replacement
               e : go restEdited restOld

      | otherwise =
          -- Old message is not editable: keep it
          o : go edited restOld
