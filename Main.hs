module Main (main) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Agent (AgentConfig, AgentSession, RunixCodeResult, runRunixCode)

-- | Main entry point for runix-code
--
-- Usage: echo "prompt" | runix-code session.json
--
-- Flow:
-- 1. Read session file path from args
-- 2. Read user input from stdin
-- 3. Load or create session
-- 4. Load config
-- 5. Run agent (Runix task)
-- 6. Display response
-- 7. Save session
main :: IO ()
main = do
  sessionFile <- getSessionFile
  userInput <- getUserInput
  state <- loadOrCreateSession sessionFile
  config <- loadConfig
  response <- runAgentTask config state userInput
  displayResponse response
  saveSession sessionFile response

--------------------------------------------------------------------------------
-- Fixed points to implement
--------------------------------------------------------------------------------

-- | Get session file from command-line args
getSessionFile :: IO FilePath
getSessionFile = undefined

-- | Read user input from stdin until EOF
getUserInput :: IO Text
getUserInput = undefined

-- | Load session from file, or create new if doesn't exist
loadOrCreateSession :: FilePath -> IO (AgentSession provider model)
loadOrCreateSession _path = undefined

-- | Load agent config (system prompt, etc.)
loadConfig :: IO AgentConfig
loadConfig = undefined

-- | Run runixCode as a Runix task
runAgentTask :: AgentConfig -> AgentSession provider model -> Text -> IO (RunixCodeResult provider model)
runAgentTask _config _state _input = undefined

-- | Display result to stdout
displayResponse :: RunixCodeResult provider model -> IO ()
displayResponse _response = undefined

-- | Save session to file
saveSession :: FilePath -> RunixCodeResult provider model -> IO ()
saveSession _path _response = undefined
