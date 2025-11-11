{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Data.Text (Text)
import UI.OutputHistory

-- Helper to create output messages
convMsg :: Text -> OutputMessage
convMsg = ConversationMessage 0

logMsg :: Text -> OutputMessage
logMsg = LogEntry Info

-- Helper to extract text for comparison
getText :: OutputMessage -> Text
getText (ConversationMessage _ t) = t
getText (LogEntry _ t) = "Log: " <> t
getText (StreamingChunk t) = "Stream: " <> t
getText (StreamingReasoning t) = "Reasoning: " <> t
getText (SystemEvent t) = "System: " <> t
getText (ToolExecution t) = "Tool: " <> t

main :: IO ()
main = hspec $ do
  describe "mergeOutputMessages" $ do
    it "keeps logs in position between conversation messages" $ do
      let oldOutput = [ logMsg "HTTP request"
                      , convMsg "User: hello"
                      ]
          newConvMsgs = [ convMsg "Agent: hi"
                        , convMsg "User: hello"
                        ]
          result = mergeOutputMessages newConvMsgs oldOutput
          actual = map getText result

      -- Log should stay between Agent and User
      actual `shouldBe` ["Agent: hi", "Log: HTTP request", "User: hello"]

    it "handles matching messages by keeping them" $ do
      let oldOutput = [ convMsg "Agent: hi"
                      , convMsg "User: hello"
                      ]
          newConvMsgs = [ convMsg "Agent: hi"
                        , convMsg "User: hello"
                        ]
          result = mergeOutputMessages newConvMsgs oldOutput
          actual = map getText result

      actual `shouldBe` ["Agent: hi", "User: hello"]

    it "handles new message insertion" $ do
      let oldOutput = [ convMsg "User: hello" ]
          newConvMsgs = [ convMsg "Agent: hi"
                        , convMsg "User: hello"
                        ]
          result = mergeOutputMessages newConvMsgs oldOutput
          actual = map getText result

      actual `shouldBe` ["Agent: hi", "User: hello"]

    it "preserves multiple logs with their messages" $ do
      let oldOutput = [ logMsg "request 2"
                      , convMsg "Agent: response"
                      , logMsg "request 1"
                      , convMsg "User: hello"
                      ]
          newConvMsgs = [ convMsg "Agent: new response"
                        , convMsg "Agent: response"
                        , convMsg "User: hello"
                        ]
          result = mergeOutputMessages newConvMsgs oldOutput
          actual = map getText result

      -- Logs should stay with their respective messages
      actual `shouldBe` [ "Agent: new response"
                        , "Log: request 2"
                        , "Agent: response"
                        , "Log: request 1"
                        , "User: hello"
                        ]
