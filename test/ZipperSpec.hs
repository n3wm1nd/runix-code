{-# LANGUAGE OverloadedStrings #-}

module ZipperSpec (spec) where

import Test.Hspec
import UI.OutputHistory
import UI.AgentWidgets (SubsectionAddr(..), AgentStatus(..))
import Runix.Logging (Level(..))
import Data.Text (Text)

-- Simple message type for testing
type TestMsg = Text

spec :: Spec
spec = do
  describe "Basic zipper operations" $ do
    it "creates an empty zipper" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
      zipperToList z `shouldBe` []

    it "appends items" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (LogItem Info "log1") z
          z2 = appendItem (LogItem Info "log2") z1
      zipperToList z2 `shouldBe` [LogItem Info "log2", LogItem Info "log1"]

    it "moves forward and backward" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (LogItem Info "log1") z
          z2 = appendItem (LogItem Info "log2") z1
          z3 = moveOlder z2
      zipperCurrent z3 `shouldBe` Just (LogItem Info "log1")

  describe "Subsection operations" $ do
    it "creates a subsection" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (SectionItem emptyZipper) z
      case zipperToList z1 of
        [SectionItem sub] -> zipperToList sub `shouldBe` ([] :: [OutputItem TestMsg])
        _ -> expectationFailure "Expected one SectionItem"

    it "adds items to root" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = atAddress Root (appendItem (LogItem Info "root log")) z
      zipperToList z1 `shouldBe` [LogItem Info "root log"]

    it "adds items to a subsection" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          -- Create a subsection
          z1 = appendItem (SectionItem emptyZipper) z
          -- Add to the subsection
          z2 = atAddress (Nested 0 Root) (appendItem (LogItem Info "sub log")) z1
      case zipperToList z2 of
        [SectionItem sub] -> zipperToList sub `shouldBe` [LogItem Info "sub log" :: OutputItem TestMsg]
        other -> expectationFailure $ "Expected SectionItem with log, got: " ++ show other

    it "handles multiple subsections" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (SectionItem emptyZipper) z  -- subsection 0
          z2 = appendItem (SectionItem emptyZipper) z1 -- subsection 1
          -- Add to first subsection (oldest)
          z3 = atAddress (Nested 0 Root) (appendItem (LogItem Info "sub0")) z2
          -- Add to second subsection (newest)
          z4 = atAddress (Nested 1 Root) (appendItem (LogItem Info "sub1")) z3
      case zipperToList z4 of
        [SectionItem sub1, SectionItem sub0] -> do
          zipperToList sub0 `shouldBe` [LogItem Info "sub0" :: OutputItem TestMsg]
          zipperToList sub1 `shouldBe` [LogItem Info "sub1" :: OutputItem TestMsg]
        other -> expectationFailure $ "Expected two SectionItems, got: " ++ show other

    it "handles nested subsections" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          -- Create subsection at root
          z1 = appendItem (SectionItem emptyZipper) z
          -- Create subsection within subsection
          z2 = atAddress (Nested 0 Root) (appendItem (SectionItem emptyZipper)) z1
          -- Add to nested subsection
          z3 = atAddress (Nested 0 (Nested 0 Root)) (appendItem (LogItem Info "nested")) z2
      case zipperToList z3 of
        [SectionItem sub] ->
          case zipperToList sub of
            [SectionItem nestedSub] ->
              zipperToList nestedSub `shouldBe` [LogItem Info "nested" :: OutputItem TestMsg]
            other -> expectationFailure $ "Expected nested SectionItem, got: " ++ show other
        other -> expectationFailure $ "Expected SectionItem, got: " ++ show other

  describe "countSubtrees" $ do
    it "counts zero subsections in empty zipper" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
      countSubtrees z `shouldBe` 0

    it "counts subsections at root" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (LogItem Info "log") z
          z2 = appendItem (SectionItem emptyZipper) z1
          z3 = appendItem (SectionItem emptyZipper) z2
      countSubtrees z3 `shouldBe` 2

    it "counts subsections regardless of position" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (SectionItem emptyZipper) z
          z2 = appendItem (SectionItem emptyZipper) z1
          z3 = moveOlder z2  -- Move to older position
      countSubtrees z3 `shouldBe` 2

  describe "queryAtAddress" $ do
    it "queries root" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (LogItem Info "log") z
          result = queryAtAddress Root countSubtrees z1
      result `shouldBe` 0

    it "queries subsection" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (SectionItem emptyZipper) z
          z2 = atAddress (Nested 0 Root) (appendItem (LogItem Info "sub")) z1
          result = queryAtAddress (Nested 0 Root) (length . zipperToList) z2
      result `shouldBe` 1

  -- describe "onRewoundZipper" $ do
  --   it "preserves position after operation" $ do
  --     let z = emptyZipper :: Zipper (OutputItem TestMsg)
  --         z1 = appendItem (LogItem Info "log1") z
  --         z2 = appendItem (LogItem Info "log2") z1
  --         z3 = appendItem (LogItem Info "log3") z2
  --         -- Current position is at log3
  --         z4 = onRewoundZipper (appendItem (LogItem Info "log0")) z3
  --     -- Should still be at log3
  --     zipperCurrent z4 `shouldBe` Just (LogItem Info "log3" :: OutputItem TestMsg)
  --     -- But log0 should be at the oldest position
  --     let items = zipperToList z4
  --     last items `shouldBe` LogItem Info "log0"

  describe "onForwardUntil" $ do
    it "finds and modifies matching element" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (LogItem Info "log1") z
          z2 = appendItem (MessageItem "msg1") z1
          z3 = appendItem (LogItem Info "log2") z2
          isMessage (MessageItem _) = True
          isMessage _ = False
          z4 = onRewoundZipper (onForwardUntil isMessage (appendItem (LogItem Info "found"))) z3
      -- Should find the message and add log before it
      let items = zipperToList z4
      items `shouldContain` [LogItem Info "found" :: OutputItem TestMsg]

    it "doesn't loop on unfound condition" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (LogItem Info "log1") z
          z2 = appendItem (LogItem Info "log2") z1
          isMessage (MessageItem _) = True
          isMessage _ = False
          -- No messages in zipper, should not loop
          z3 = onRewoundZipper (onForwardUntil isMessage (appendItem (LogItem Info "found"))) z2
      -- Should be unchanged
      zipperToList z3 `shouldBe` (zipperToList z2 :: [OutputItem TestMsg])

  describe "extractMessages" $ do
    it "extracts nothing from empty zipper" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
      extractMessages z `shouldBe` ([] :: [TestMsg])

    it "extracts single message" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (MessageItem "msg1") z
      extractMessages z1 `shouldBe` ["msg1"]

    it "extracts multiple messages in oldest-first order" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (MessageItem "msg1") z    -- oldest
          z2 = appendItem (MessageItem "msg2") z1
          z3 = appendItem (MessageItem "msg3") z2   -- newest
      extractMessages z3 `shouldBe` ["msg1", "msg2", "msg3"]

    it "skips non-message items (logs, status, etc)" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (MessageItem "msg1") z
          z2 = appendItem (LogItem Info "log1") z1
          z3 = appendItem (MessageItem "msg2") z2
          z4 = appendItem (StatusItem Idle) z3
          z5 = appendItem (MessageItem "msg3") z4
          z6 = appendItem (SystemEventItem "event") z5
      extractMessages z6 `shouldBe` ["msg1", "msg2", "msg3"]

    it "extracts messages from subsection" $ do
      let subZ = emptyZipper :: Zipper (OutputItem TestMsg)
          subZ1 = appendItem (MessageItem "submsg1") subZ
          subZ2 = appendItem (MessageItem "submsg2") subZ1
          z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (SectionItem subZ2) z
      extractMessages z1 `shouldBe` ["submsg1", "submsg2"]

    it "extracts messages from root and subsection in order" $ do
      let subZ = emptyZipper :: Zipper (OutputItem TestMsg)
          subZ1 = appendItem (MessageItem "submsg1") subZ
          subZ2 = appendItem (MessageItem "submsg2") subZ1
          z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (MessageItem "msg1") z        -- oldest at root
          z2 = appendItem (SectionItem subZ2) z1        -- subsection
          z3 = appendItem (MessageItem "msg2") z2       -- newest at root
      -- Order should be: root oldest, subsection messages, root newest
      extractMessages z3 `shouldBe` ["msg1", "submsg1", "submsg2", "msg2"]

    it "extracts messages from multiple subsections in order" $ do
      let subZ1 = emptyZipper :: Zipper (OutputItem TestMsg)
          subZ1a = appendItem (MessageItem "sub1msg1") subZ1
          subZ1b = appendItem (MessageItem "sub1msg2") subZ1a
          subZ2 = emptyZipper :: Zipper (OutputItem TestMsg)
          subZ2a = appendItem (MessageItem "sub2msg1") subZ2
          subZ2b = appendItem (MessageItem "sub2msg2") subZ2a
          z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (MessageItem "msg1") z
          z2 = appendItem (SectionItem subZ1b) z1       -- first subsection (older)
          z3 = appendItem (MessageItem "msg2") z2
          z4 = appendItem (SectionItem subZ2b) z3       -- second subsection (newer)
          z5 = appendItem (MessageItem "msg3") z4
      extractMessages z5 `shouldBe`
        ["msg1", "sub1msg1", "sub1msg2", "msg2", "sub2msg1", "sub2msg2", "msg3"]

    it "extracts messages from nested subsections" $ do
      let nestedZ = emptyZipper :: Zipper (OutputItem TestMsg)
          nestedZ1 = appendItem (MessageItem "nestedmsg") nestedZ
          subZ = emptyZipper :: Zipper (OutputItem TestMsg)
          subZ1 = appendItem (MessageItem "submsg1") subZ
          subZ2 = appendItem (SectionItem nestedZ1) subZ1
          subZ3 = appendItem (MessageItem "submsg2") subZ2
          z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (MessageItem "msg1") z
          z2 = appendItem (SectionItem subZ3) z1
          z3 = appendItem (MessageItem "msg2") z2
      extractMessages z3 `shouldBe`
        ["msg1", "submsg1", "nestedmsg", "submsg2", "msg2"]

    it "handles empty subsections" $ do
      let emptySubZ = emptyZipper :: Zipper (OutputItem TestMsg)
          z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (MessageItem "msg1") z
          z2 = appendItem (SectionItem emptySubZ) z1
          z3 = appendItem (MessageItem "msg2") z2
      extractMessages z3 `shouldBe` ["msg1", "msg2"]

    it "handles subsections with only logs (no messages)" $ do
      let subZ = emptyZipper :: Zipper (OutputItem TestMsg)
          subZ1 = appendItem (LogItem Info "log1") subZ
          subZ2 = appendItem (LogItem Info "log2") subZ1
          z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (MessageItem "msg1") z
          z2 = appendItem (SectionItem subZ2) z1
          z3 = appendItem (MessageItem "msg2") z2
      extractMessages z3 `shouldBe` ["msg1", "msg2"]

    it "preserves order when zipper focus is not at newest" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (MessageItem "msg1") z
          z2 = appendItem (MessageItem "msg2") z1
          z3 = appendItem (MessageItem "msg3") z2
          -- Move focus to older message
          z4 = moveOlder z3
          z5 = moveOlder z4
      -- Should still extract in oldest-first order regardless of focus
      extractMessages z5 `shouldBe` ["msg1", "msg2", "msg3"]

    it "handles zipper with items in back, current, and front" $ do
      let z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (MessageItem "msg1") z
          z2 = appendItem (MessageItem "msg2") z1
          z3 = appendItem (MessageItem "msg3") z2
          z4 = appendItem (MessageItem "msg4") z3
          -- Move focus to middle
          z5 = moveOlder z4  -- focus on msg3
          z6 = moveOlder z5  -- focus on msg2
      -- Now: back=[msg4, msg3], current=msg2, front=[msg1]
      extractMessages z6 `shouldBe` ["msg1", "msg2", "msg3", "msg4"]

    it "handles complex nested structure with mixed items" $ do
      -- Create a complex structure:
      -- msg1
      -- log1
      -- section1:
      --   submsg1
      --   log2
      --   section1.1:
      --     nestedmsg1
      --   submsg2
      -- msg2
      -- section2:
      --   submsg3
      -- msg3
      let nested1_1 = emptyZipper :: Zipper (OutputItem TestMsg)
          nested1_1a = appendItem (MessageItem "nestedmsg1") nested1_1
          section1 = emptyZipper :: Zipper (OutputItem TestMsg)
          section1a = appendItem (MessageItem "submsg1") section1
          section1b = appendItem (LogItem Info "log2") section1a
          section1c = appendItem (SectionItem nested1_1a) section1b
          section1d = appendItem (MessageItem "submsg2") section1c
          section2 = emptyZipper :: Zipper (OutputItem TestMsg)
          section2a = appendItem (MessageItem "submsg3") section2
          z = emptyZipper :: Zipper (OutputItem TestMsg)
          z1 = appendItem (MessageItem "msg1") z
          z2 = appendItem (LogItem Info "log1") z1
          z3 = appendItem (SectionItem section1d) z2
          z4 = appendItem (MessageItem "msg2") z3
          z5 = appendItem (SectionItem section2a) z4
          z6 = appendItem (MessageItem "msg3") z5
      extractMessages z6 `shouldBe`
        ["msg1", "submsg1", "nestedmsg1", "submsg2", "msg2", "submsg3", "msg3"]
