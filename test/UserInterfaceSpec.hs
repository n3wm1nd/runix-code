{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module UserInterfaceSpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.State (runState, get, put)

import Runix.LLM (LLM(..))
import Runix.Logging (Logging(..))
import UI.AgentWidgets (AgentWidgets(..), AgentStatus(..), SubsectionAddr(..))
import UI.UserInterface (interpretAsWidget)
import Polysemy.Fail (Fail, runFail)
import UniversalLLM (Message(..))

-- | Test that LLM results are forwarded to AgentWidgets
spec :: Spec
spec = describe "interpretAsWidget" $ do
  it "forwards LLM results to AgentWidgets" $ do
    -- Collect AgentWidgets outputs
    let collectOutputs :: Sem (AgentWidgets (Message String) : r) a -> Sem r (([AgentStatus], [Message String]), a)
        collectOutputs = runState ([], []) . reinterpret (\case
          AddMessage _addr msg -> do
            (statuses, msgs) <- get @([AgentStatus], [Message String])
            put (statuses, msgs ++ [msg])
          LogMessage _addr _ _ -> return ()
          SetStatus _addr status -> do
            (statuses, msgs) <- get @([AgentStatus], [Message String])
            put (statuses ++ [status], msgs)
          ReplaceHistory _addr _ -> return ()
          StartSubsection _addr -> return Root  -- Mock subsection creation
          )

    -- Mock LLM that returns a simple response
    let mockLLM = interpret @(LLM String) $ \case
          QueryLLM _configs _msgs -> do
            return (Right [AssistantText "Hello, World!"])

    -- Mock Logging (no-op)
    let mockLogging = interpret @Logging $ \case
          Log _ _ _ -> return ()

    -- Build the test program - just calls LLM
    let testProgram = send (QueryLLM @String [] [])

    let result = run
               $ runFail                   -- provides Fail on r
               $ mockLLM                   -- provides outer LLM on r
               $ collectOutputs            -- provides outer AgentWidgets on r
               $ interpretAsWidget @String -- interprets inner AgentWidgets, Logging, LLM
               $ testProgram

    case result of
      Right ((statuses, messages), _) -> do
        -- Should have a Done status
        statuses `shouldContain` [Done]

        -- Should have the assistant message
        messages `shouldBe` [AssistantText "Hello, World!"]

      Left err -> expectationFailure $ "Test failed: " ++ err
