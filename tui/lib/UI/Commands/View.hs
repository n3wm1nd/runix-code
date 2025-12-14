{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | /view command - View conversation history in $PAGER
module UI.Commands.View
  ( viewCommand
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.IORef
import System.Environment (lookupEnv)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (spawnProcess, waitForProcess)
import Polysemy
import Runix.Logging.Effects (Logging, info)
import UniversalLLM.Core.Types (Message(..), ToolCall(..), ToolResult(..))
import UI.State (UIVars, sendAgentEvent, AgentEvent(..))

-- | /view slash command
viewCommand :: forall model msg r. Members '[Embed IO, Logging] r
            => IORef [Message model]
            -> UIVars msg
            -> (T.Text, T.Text -> Sem r ())
viewCommand historyRef uiVars = ("view", \_ -> do
  currentHistory <- embed $ readIORef historyRef
  embed $ sendAgentEvent uiVars (RunExternalCommandEvent (viewHistoryInPager currentHistory))
  info "Opened history in pager")

-- | View conversation history in $PAGER (blocks until pager exits)
viewHistoryInPager :: [Message model] -> IO ()
viewHistoryInPager messages = do
  -- Get pager from environment, default to 'less'
  maybePager <- lookupEnv "PAGER"
  let pager = maybe "less" id maybePager
      -- Parse pager into command and args (simple split on spaces)
      (cmd, cmdArgs) = case words pager of
        [] -> ("less", [])  -- Fallback if empty
        (c:rest) -> (c, rest)

  -- Format all messages
  let content = T.concat $ map formatMessage messages

  -- Write to temp file and run pager inside withSystemTempFile (blocks until done)
  withSystemTempFile "runix-code-history.txt" $ \path handle -> do
    TIO.hPutStr handle content
    hClose handle
    -- Run pager and wait for it to complete
    ph <- spawnProcess cmd (cmdArgs ++ [path])
    _ <- waitForProcess ph
    return ()

-- | Format a message for display in pager
formatMessage :: Message model -> T.Text
formatMessage (UserText text) = "=== USER ===\n" <> text <> "\n"
formatMessage (AssistantText text) = "=== ASSISTANT ===\n" <> text <> "\n"
formatMessage (AssistantReasoning text) = "=== REASONING ===\n" <> text <> "\n"
formatMessage (AssistantTool (ToolCall _ name args)) =
  "=== TOOL_CALL ===\n" <> name <> "\n" <> T.pack (show args) <> "\n"
formatMessage (AssistantTool (InvalidToolCall _ name args err)) =
  "=== INVALID_TOOL_CALL ===\n" <> name <> "\nArgs: " <> T.pack (show args) <> "\nError: " <> err <> "\n"
formatMessage (ToolResultMsg (ToolResult _ result)) =
  "=== TOOL_RESULT ===\n" <> T.pack (show result) <> "\n"
formatMessage (SystemText text) = "=== SYSTEM ===\n" <> text <> "\n"
formatMessage (UserImage desc _) = "=== USER_IMAGE ===\n" <> desc <> "\n"
formatMessage (UserRequestJSON query schema) =
  "=== USER_JSON ===\nQuery: " <> query <> "\nSchema: " <> T.pack (show schema) <> "\n"
formatMessage (AssistantJSON value) =
  "=== ASSISTANT_JSON ===\n" <> T.pack (show value) <> "\n"
