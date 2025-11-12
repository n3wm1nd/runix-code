{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub, sort)
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

isConv :: OutputMessage -> Bool
isConv (ConversationMessage _ _) = True
isConv _ = False

extractConvs :: [OutputMessage] -> [OutputMessage]
extractConvs = filter isConv

--------------------------------------------------------------------------------
-- Arbitrary instances for QuickCheck
--------------------------------------------------------------------------------

arbitraryText :: Gen Text
arbitraryText = elements
  [ "User: hello"
  , "Agent: hi"
  , "User: how are you?"
  , "Agent: I'm fine"
  , "User: goodbye"
  , "Agent: bye"
  , "HTTP request"
  , "File read"
  , "Command executed"
  , "Cache hit"
  ]

instance Arbitrary OutputMessage where
  arbitrary = oneof
    [ ConversationMessage 0 <$> arbitraryConvText
    , LogEntry Info <$> arbitraryLogText
    , StreamingChunk <$> arbitraryText
    , SystemEvent <$> arbitraryText
    , ToolExecution <$> arbitraryText
    ]
    where
      arbitraryConvText = elements
        [ "User: hello"
        , "Agent: hi"
        , "User: how are you?"
        , "Agent: I'm fine"
        , "User: goodbye"
        , "Agent: bye"
        ]
      arbitraryLogText = elements
        [ "HTTP request"
        , "File read"
        , "Command executed"
        , "Cache hit"
        ]

-- Generate a valid old output history (conversations + logs, newest first)
genOldOutput :: Gen [OutputMessage]
genOldOutput = do
  convs <- listOf1 arbitrary `suchThat` (all isConv)
  let uniqueConvs = reverse $ nub $ reverse convs  -- Keep newest duplicates
  insertLogs uniqueConvs
  where
    insertLogs [] = return []
    insertLogs (c:cs) = do
      logsBefore <- listOf (arbitrary `suchThat` (not . isConv))
      rest <- insertLogs cs
      return (logsBefore ++ [c] ++ rest)

-- Generate new conversation messages (subset/superset of old conversations)
genNewConvs :: [OutputMessage] -> Gen [OutputMessage]
genNewConvs oldOutput = do
  let oldConvs = extractConvs oldOutput
  -- Either keep all, add some new, or remove some
  newOnes <- listOf (arbitrary `suchThat` isConv)
  elements
    [ oldConvs  -- No change
    , reverse $ nub $ reverse (newOnes ++ oldConvs)  -- Add new messages
    , if null oldConvs then [] else take (length oldConvs - 1) oldConvs  -- Remove last
    ]

--------------------------------------------------------------------------------
-- Unit tests (original examples)
--------------------------------------------------------------------------------

unitTests :: Spec
unitTests = describe "mergeOutputMessages (unit tests)" $ do
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

  it "handles non-conversation in newItems by prepending them" $ do
    let oldOutput = [ logMsg "old log"
                    , convMsg "User: hello"
                    ]
        -- Contract violation: newItems should only have conversations
        newItems = [ logMsg "new log"
                   , convMsg "Agent: hi"
                   , convMsg "User: hello"
                   ]
        result = mergeOutputMessages newItems oldOutput
        actual = map getText result

    -- Non-convs in newItems get prepended, old non-convs preserved
    actual `shouldBe` [ "Log: new log"
                      , "Agent: hi"
                      , "Log: old log"
                      , "User: hello"
                      ]

  it "handles empty old output" $ do
    let oldOutput = []
        newConvMsgs = [ convMsg "Agent: hi"
                      , convMsg "User: hello"
                      ]
        result = mergeOutputMessages newConvMsgs oldOutput
        actual = map getText result

    actual `shouldBe` ["Agent: hi", "User: hello"]

  it "handles empty new messages" $ do
    let oldOutput = [ logMsg "old log"
                    , convMsg "User: hello"
                    ]
        newConvMsgs = []
        result = mergeOutputMessages newConvMsgs oldOutput
        actual = map getText result

    -- All conversations removed, only logs remain
    actual `shouldBe` ["Log: old log"]

  it "handles message deletion" $ do
    let oldOutput = [ convMsg "Agent: first"
                    , logMsg "log 1"
                    , convMsg "User: hello"
                    ]
        -- User message is removed in new history
        newConvMsgs = [ convMsg "Agent: first" ]
        result = mergeOutputMessages newConvMsgs oldOutput
        actual = map getText result

    -- User message gone, but logs preserved
    actual `shouldBe` ["Agent: first", "Log: log 1"]

  it "handles message reordering (shouldn't happen but test behavior)" $ do
    let oldOutput = [ convMsg "Agent: second"
                    , convMsg "User: first"
                    ]
        -- Messages appear in different order
        newConvMsgs = [ convMsg "User: first"
                      , convMsg "Agent: second"
                      ]
        result = mergeOutputMessages newConvMsgs oldOutput
        actual = map getText result

    -- New order is used
    actual `shouldBe` ["User: first", "Agent: second"]

  it "handles duplicate messages" $ do
    let oldOutput = [ convMsg "Agent: hi"
                    , logMsg "log"
                    , convMsg "Agent: hi"  -- Duplicate
                    , convMsg "User: hello"
                    ]
        newConvMsgs = [ convMsg "Agent: hi"
                      , convMsg "User: hello"
                      ]
        result = mergeOutputMessages newConvMsgs oldOutput
        actual = map getText result

    -- Duplicates handled - first match wins
    actual `shouldBe` ["Agent: hi", "Log: log", "User: hello"]

  it "handles only non-conversation messages in old output" $ do
    let oldOutput = [ logMsg "log 1"
                    , logMsg "log 2"
                    ]
        newConvMsgs = [ convMsg "Agent: hi" ]
        result = mergeOutputMessages newConvMsgs oldOutput
        actual = map getText result

    -- Logs at the end, new message at start
    actual `shouldBe` ["Log: log 1", "Log: log 2", "Agent: hi"]

--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

properties :: Spec
properties = describe "mergeOutputMessages (properties)" $ do

  it "preserves all new conversation messages" $ property $
    forAll genOldOutput $ \oldOutput ->
    forAll (genNewConvs oldOutput) $ \newConvs ->
      let result = mergeOutputMessages newConvs oldOutput
          resultConvs = extractConvs result
      in resultConvs == newConvs

  it "preserves all non-conversation messages from old output" $ property $
    forAll genOldOutput $ \oldOutput ->
    forAll (genNewConvs oldOutput) $ \newConvs ->
      let result = mergeOutputMessages newConvs oldOutput
          oldLogs = filter (not . isConv) oldOutput
          resultLogs = filter (not . isConv) result
      in sort oldLogs == sort resultLogs

  it "maintains relative order of non-conversation messages" $ property $
    forAll genOldOutput $ \oldOutput ->
    forAll (genNewConvs oldOutput) $ \newConvs ->
      let result = mergeOutputMessages newConvs oldOutput
          oldLogs = filter (not . isConv) oldOutput
          resultLogs = filter (not . isConv) result
          -- Extract subsequence of logs from result
          isSubsequence [] _ = True
          isSubsequence _ [] = False
          isSubsequence (x:xs) (y:ys)
            | x == y = isSubsequence xs ys
            | otherwise = isSubsequence (x:xs) ys
      in isSubsequence oldLogs resultLogs

  it "when all conversation messages match, output is unchanged" $ property $
    forAll genOldOutput $ \oldOutput ->
      let convs = extractConvs oldOutput
          result = mergeOutputMessages convs oldOutput
      in result == oldOutput

  it "result length is sum of new convs and old non-convs" $ property $
    forAll genOldOutput $ \oldOutput ->
    forAll (genNewConvs oldOutput) $ \newConvs ->
      let result = mergeOutputMessages newConvs oldOutput
          oldNonConvs = filter (not . isConv) oldOutput
      in length result == length newConvs + length oldNonConvs

  it "handles non-conversation messages in newItems gracefully (contract violation)" $ property $
    forAll genOldOutput $ \oldOutput ->
    forAll (listOf arbitrary) $ \newItems ->  -- Can include non-convs
      let result = mergeOutputMessages newItems oldOutput
          -- Function should not crash - just check we get a result
          -- Behavior is undefined for contract violations, but shouldn't crash
      in length result >= 0  -- Always true, but forces evaluation

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  unitTests
  properties
