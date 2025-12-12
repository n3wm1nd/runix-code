{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Claude Code integrations (subagents and skills)
module Tools.Claude
  ( -- * Subagents
    loadSubagents
  , claudeSubagentToTool
  -- * Skills
  , loadSkills
  , claudeSkillToTool
  -- * CLAUDE.md
  , loadClaudeMdConfigs
  , ClaudeInstructions (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (catMaybes)
import System.FilePath ((</>), takeDirectory, takeFileName)
import Polysemy (Member, Members, Sem, raise)
import Polysemy.State (State, get, put, modify)
import Polysemy.Fail (Fail, runFail)
import UniversalLLM.Core.Types (Message(..))
import UniversalLLM.Core.Tools (LLMTool(..), mkTool, llmToolToDefinition, ToolParameter(..))
import UniversalLLM (HasTools, SupportsSystemPrompt, ProviderOf)
import qualified UniversalLLM as ULL
import Runix.LLM.Effects (LLM, queryLLM)
import Runix.LLM.ToolInstances ()
import Runix.Cmd.Effects (Cmd, cmdExec, CmdOutput(..))
import Runix.Logging.Effects (Logging)
import Runix.LLM.ToolExecution (executeTool)
import qualified Runix.FileSystem.Effects
import Autodocodec (HasCodec, codec)
import qualified Autodocodec

--------------------------------------------------------------------------------
-- Shared Types and Functions
--------------------------------------------------------------------------------

-- | User prompt - what the user wants
newtype UserPrompt = UserPrompt Text
  deriving stock (Show, Eq)

instance HasCodec UserPrompt where
  codec = Autodocodec.dimapCodec UserPrompt (\(UserPrompt t) -> t) codec

instance ToolParameter UserPrompt where
  paramName _ _ = "prompt"
  paramDescription _ = "the user's request or question"

-- | CLAUDE.md instructions
newtype ClaudeInstructions = ClaudeInstructions Text
  deriving stock (Show, Eq)

-- | Result from a subagent/skill call
data SubagentResult model = SubagentResult
  { subagentResponse :: Text
  } deriving stock (Show)

instance HasCodec (SubagentResult model) where
  codec = Autodocodec.dimapCodec
    (\txt -> SubagentResult txt)
    subagentResponse
    codec

instance ToolParameter (SubagentResult model) where
  paramName _ _ = "result"
  paramDescription _ = "result from the subagent/skill"

-- | Tools mode for Claude agents
data ToolsMode
  = AllTools
  | SpecificTools [Text]
  deriving stock (Show, Eq)

-- | Claude Code subagent definition
data ClaudeAgent = ClaudeAgent
  { agentName :: Text
  , agentDescription :: Text
  , agentTools :: ToolsMode
  , agentSystemPrompt :: Text
  , agentColor :: Maybe Text
  } deriving stock (Show, Eq)

-- | Claude Code skill definition
data ClaudeSkill = ClaudeSkill
  { skillName :: Text
  , skillDescription :: Text
  , skillSystemPrompt :: Text
  , skillBasePath :: FilePath
  } deriving stock (Show, Eq)

-- | Shared agent loop for subagents and skills
subagentLoop
  :: forall model r.
     ( Member (LLM model) r
     , Member Logging r
     , Member (State [Message model]) r
     , HasTools model
     )
  => [ULL.ModelConfig model]
  -> [LLMTool (Sem (Fail ': r))]
  -> Sem r (SubagentResult model)
subagentLoop configs tools = do
  let configsWithTools = setTools tools configs

  history <- get @[Message model]
  responseMsgs <- queryLLM configsWithTools history

  let updatedHistory = history ++ responseMsgs
      toolCalls = [tc | AssistantTool tc <- responseMsgs]

  put @[Message model] updatedHistory

  case toolCalls of
    [] -> do
      let assistantResponse = case [txt | AssistantText txt <- responseMsgs] of
            (txt:_) -> txt
            [] -> ""
      return $ SubagentResult assistantResponse

    calls -> do
      results <- mapM (executeTool tools) calls
      let historyWithResults = updatedHistory ++ map ToolResultMsg results
      put @[Message model] historyWithResults
      subagentLoop @model configsWithTools tools

-- | Update config with new tool list
setTools :: HasTools model => [LLMTool (Sem r)] -> [ULL.ModelConfig model] -> [ULL.ModelConfig model]
setTools tools configs =
  let withoutTools = filter (not . isToolsConfig) configs
      toolDefs = map llmToolToDefinition tools
  in withoutTools ++ [ULL.Tools toolDefs]
  where
    isToolsConfig (ULL.Tools _) = True
    isToolsConfig _ = False

--------------------------------------------------------------------------------
-- Subagents
--------------------------------------------------------------------------------

-- | Load all subagent definitions from .claude/agents/*.md
loadSubagents
  :: Members '[Runix.FileSystem.Effects.FileSystemRead, Runix.FileSystem.Effects.FileSystemWrite] r
  => Sem r [ClaudeAgent]
loadSubagents = do
  result <- runFail $ Runix.FileSystem.Effects.glob ".claude/agents" "*.md"
  case result of
    Left _ -> return []
    Right files -> catMaybes <$> mapM parseAgentFile files
  where
    parseAgentFile :: Member Runix.FileSystem.Effects.FileSystemRead r => FilePath -> Sem r (Maybe ClaudeAgent)
    parseAgentFile path = do
      contentResult <- runFail $ Runix.FileSystem.Effects.readFile path
      case contentResult of
        Left _ -> return Nothing
        Right contents -> return $ parseAgentMarkdown (TE.decodeUtf8 contents)

-- | Parse agent markdown with YAML frontmatter
parseAgentMarkdown :: Text -> Maybe ClaudeAgent
parseAgentMarkdown content =
  case T.splitOn "---" content of
    (_ : frontmatter : body : _) -> do
      name <- extractField "name:" frontmatter
      description <- extractField "description:" frontmatter
      let toolsText = extractField "tools:" frontmatter
          tools = maybe AllTools (SpecificTools . map T.strip . T.splitOn ",") toolsText
          color = extractField "color:" frontmatter
          systemPrompt = T.strip body
      return $ ClaudeAgent name description tools systemPrompt color
    _ -> Nothing
  where
    extractField :: Text -> Text -> Maybe Text
    extractField fieldName text =
      case filter (T.isPrefixOf fieldName) (T.lines text) of
        (line:_) -> Just $ T.strip $ T.drop (T.length fieldName) line
        [] -> Nothing

-- | Convert a ClaudeAgent to an LLMTool
claudeSubagentToTool
  :: forall model r.
     ( Member (LLM model) r
     , Member Logging r
     , Member (State [Message model]) r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => [LLMTool (Sem (Fail ': r))]
  -> ClaudeAgent
  -> LLMTool (Sem (Fail ': r))
claudeSubagentToTool tools agent =
  let toolFunction = raise . claudeSubagent @model tools agent
      wrapped = mkTool (agentName agent) (agentDescription agent) toolFunction
  in LLMTool wrapped

-- | Run a subagent with fresh history (isolated)
claudeSubagent
  :: forall model r.
     ( Member (LLM model) r
     , Member Logging r
     , Member (State [Message model]) r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => [LLMTool (Sem (Fail ': r))]
  -> ClaudeAgent
  -> UserPrompt
  -> Sem r (SubagentResult model)
claudeSubagent tools agent (UserPrompt userPrompt) = do
  let systemPrompt = agentSystemPrompt agent
      configs = [ULL.SystemPrompt systemPrompt]
      initialHistory = [UserText userPrompt]

  -- Save current history and run subagent with fresh history
  savedHistory <- get @[Message model]
  put @[Message model] initialHistory
  result <- subagentLoop @model configs tools
  -- Restore original history
  put @[Message model] savedHistory
  return result

--------------------------------------------------------------------------------
-- Skills
--------------------------------------------------------------------------------

-- | Load all skill definitions from .claude/skills/*/SKILL.md
loadSkills
  :: Members '[Runix.FileSystem.Effects.FileSystemRead, Runix.FileSystem.Effects.FileSystemWrite] r
  => Sem r [ClaudeSkill]
loadSkills = do
  result <- runFail $ Runix.FileSystem.Effects.glob ".claude/skills" "*/SKILL.md"
  case result of
    Left _ -> return []
    Right files -> catMaybes <$> mapM parseSkillFile files
  where
    parseSkillFile :: Member Runix.FileSystem.Effects.FileSystemRead r => FilePath -> Sem r (Maybe ClaudeSkill)
    parseSkillFile path = do
      contentResult <- runFail $ Runix.FileSystem.Effects.readFile path
      case contentResult of
        Left _ -> return Nothing
        Right contents -> do
          let basePath = takeDirectory path
          return $ parseSkillMarkdown basePath (TE.decodeUtf8 contents)

-- | Parse skill markdown with YAML frontmatter
parseSkillMarkdown :: FilePath -> Text -> Maybe ClaudeSkill
parseSkillMarkdown basePath content =
  case T.splitOn "---" content of
    (_ : frontmatter : body : _) -> do
      name <- extractField "name:" frontmatter
      description <- extractField "description:" frontmatter
      let systemPrompt = T.strip body
      return $ ClaudeSkill name description systemPrompt basePath
    _ -> Nothing
  where
    extractField :: Text -> Text -> Maybe Text
    extractField fieldName text =
      case filter (T.isPrefixOf fieldName) (T.lines text) of
        (line:_) -> Just $ T.strip $ T.drop (T.length fieldName) line
        [] -> Nothing

-- | Load script tools for a skill from its scripts/ directory
loadSkillScripts
  :: forall r.
     ( Member Cmd r
     , Members '[Runix.FileSystem.Effects.FileSystemRead, Runix.FileSystem.Effects.FileSystemWrite] r
     )
  => ClaudeSkill
  -> Sem r [LLMTool (Sem (Fail ': r))]
loadSkillScripts skill = do
  let scriptsPath = skillBasePath skill </> "scripts"
  result <- runFail $ Runix.FileSystem.Effects.glob scriptsPath "*"
  case result of
    Left _ -> return []
    Right scriptPaths -> return $ map makeScriptTool scriptPaths
  where
    makeScriptTool :: FilePath -> LLMTool (Sem (Fail ': r))
    makeScriptTool scriptPath =
      let scriptName = T.pack $ takeFileName scriptPath
          toolName = "script_" <> scriptName
          toolDesc = "Execute " <> scriptName <> " script from " <> skillName skill <> " skill"
          toolFn :: Text -> Sem (Fail ': r) Text
          toolFn argsText = do
            let args = map T.unpack $ T.words argsText
            output <- raise $ cmdExec scriptPath args
            return $ stdout output
      in LLMTool $ mkTool toolName toolDesc toolFn

-- | Run a skill with existing history (not isolated)
claudeSkill
  :: forall model r.
     ( Member (LLM model) r
     , Member Logging r
     , Member (State [Message model]) r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => [LLMTool (Sem (Fail ': r))]
  -> ClaudeSkill
  -> UserPrompt
  -> Sem r (SubagentResult model)
claudeSkill scriptTools skill (UserPrompt userPrompt) = do
  let systemPrompt = skillSystemPrompt skill
      configs = [ULL.SystemPrompt systemPrompt]
  modify @[Message model] (++ [UserText userPrompt])
  subagentLoop @model configs scriptTools

-- | Convert a ClaudeSkill to an LLMTool
claudeSkillToTool
  :: forall model r.
     ( Member (LLM model) r
     , Member Logging r
     , Member (State [Message model]) r
     , Member Cmd r
     , Members '[Runix.FileSystem.Effects.FileSystemRead, Runix.FileSystem.Effects.FileSystemWrite] r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => ClaudeSkill
  -> Sem r (LLMTool (Sem (Fail ': r)))
claudeSkillToTool skill = do
  scriptTools <- loadSkillScripts @r skill
  let toolFunction = raise . claudeSkill @model scriptTools skill
      wrapped = mkTool (skillName skill) (skillDescription skill) toolFunction
  return $ LLMTool wrapped

--------------------------------------------------------------------------------
-- CLAUDE.md Loading
--------------------------------------------------------------------------------

-- | Load CLAUDE.md files from current directory and .claude/ directory
-- Silently ignores files that can't be read
loadClaudeMdConfigs
  :: Member Runix.FileSystem.Effects.FileSystemRead r
  => Sem r [ClaudeInstructions]
loadClaudeMdConfigs = do
  rootClaudeMd <- tryReadFile "CLAUDE.md"
  dotClaudeMd <- tryReadFile ".claude/CLAUDE.md"

  return $ catMaybes
    [ fmap (\content -> ClaudeInstructions $ "# Project Instructions (CLAUDE.md)\n\n" <> content) rootClaudeMd
    , fmap (\content -> ClaudeInstructions $ "# Additional Instructions (.claude/CLAUDE.md)\n\n" <> content) dotClaudeMd
    ]
  where
    tryReadFile :: Member Runix.FileSystem.Effects.FileSystemRead r => FilePath -> Sem r (Maybe Text)
    tryReadFile path = do
      result <- runFail $ Runix.FileSystem.Effects.readFile path
      return $ case result of
        Right contents -> Just (TE.decodeUtf8 contents)
        Left _ -> Nothing
