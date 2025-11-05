# Runix Code - Design Document

## Vision

A Claude Code clone built on Runix that provides **feature parity with Claude Code** while adding unique capabilities like self-improvement, compile-time safety guarantees, and customizable agent behavior.

**Core Principles:**
- Start with Claude Code's proven UX and prompts
- Match or exceed Claude Code's capabilities
- Add self-improvement as differentiating feature
- Maintain type safety and effect constraints
- Support both local and cloud LLM providers

---

## Feature Parity Requirements

### Core Capabilities (Must-Have for MVP)

#### 1. File Operations
**Claude Code has:**
- `Read` - Read files with line numbers, support for images/PDFs/notebooks
- `Write` - Create new files (with safety checks)
- `Edit` - String-based find/replace with exact matching
- `Glob` - Fast file pattern matching (`**/*.js`, etc.)
- `Grep` - Ripgrep-based content search with regex

**Runix Code needs:**
```haskell
data FileOps m a where
  -- Read file with optional line range
  ReadFile :: FilePath -> Maybe (Int, Int) -> FileOps m Text

  -- Read image/PDF as visual content
  ReadImage :: FilePath -> FileOps m Image
  ReadPDF :: FilePath -> FileOps m [Page]
  ReadNotebook :: FilePath -> FileOps m Notebook

  -- Write new file (errors if exists without prior read)
  WriteFile :: FilePath -> Text -> FileOps m ()

  -- Edit existing file (exact string replacement)
  EditFile :: FilePath -> Text -> Text -> Bool -> FileOps m ()
  --         file path     old      new     replaceAll

  -- Pattern matching
  Glob :: Pattern -> Maybe FilePath -> FileOps m [FilePath]

  -- Content search
  Grep :: GrepParams -> FileOps m GrepResults

data GrepParams = GrepParams
  { pattern :: Text
  , path :: Maybe FilePath
  , caseInsensitive :: Bool
  , outputMode :: OutputMode  -- content | files_with_matches | count
  , context :: Maybe (Int, Int, Int)  -- -A, -B, -C
  , showLineNumbers :: Bool
  , globFilter :: Maybe Pattern
  , typeFilter :: Maybe FileType
  , headLimit :: Maybe Int
  , multiline :: Bool
  }
```

**Status:** ✅ Most of FileSystem effect exists, need Edit implementation

---

#### 2. Shell Execution
**Claude Code has:**
- `Bash` - Execute commands with timeout, persistent sessions
- Background execution with output monitoring
- Proper error handling and quoting
- Git workflow integration (commit, PR creation)

**Runix Code needs:**
```haskell
data Shell m a where
  -- Execute command with timeout
  RunCommand :: Text -> Maybe Timeout -> Shell m CommandResult

  -- Background execution
  RunBackground :: Text -> Shell m ShellId

  -- Monitor background process
  GetOutput :: ShellId -> Maybe Regex -> Shell m Output

  -- Kill background process
  KillShell :: ShellId -> Shell m ()

data CommandResult = CommandResult
  { stdout :: Text
  , stderr :: Text
  , exitCode :: Int
  , duration :: NominalDiffTime
  }
```

**Git Integration:**
```haskell
data Git m a where
  GitStatus :: Git m Status
  GitDiff :: Git m Text
  GitCommit :: Text -> Git m ()
  GitReset :: Git m ()
  GitBranch :: Text -> Git m ()
  GitCheckout :: Text -> Git m ()
  GitPush :: Maybe Remote -> Git m ()

  -- High-level workflows
  CreateCommit :: [FilePath] -> Text -> Git m CommitHash
  CreatePR :: PRParams -> Git m PRURL

data PRParams = PRParams
  { title :: Text
  , body :: Text
  , baseBranch :: Maybe Text
  }
```

**Status:** ⚠️ Bash effect needs extension for background jobs, Git effect needs implementation

---

#### 3. LLM Integration
**Claude Code has:**
- Multi-provider support (Anthropic, OpenAI, custom)
- Tool calling with XML and JSON formats
- Streaming responses
- Reasoning/thinking tokens
- Configurable temperature, max tokens, system prompts

**Runix Code needs:**
```haskell
-- Already have this! Just need to expose properly
type LLMEffects provider model = '[LLM provider model, Logging]

-- Agent loop configuration
data AgentConfig = AgentConfig
  { systemPrompt :: Text
  , tools :: [Tool]
  , maxIterations :: Int
  , temperature :: Double
  , maxTokens :: Maybe Int
  , thinkingEnabled :: Bool
  }
```

**Status:** ✅ Already implemented via universal-llm integration

---

#### 4. Task/Agent System
**Claude Code has:**
- `Task` - Spawn specialized sub-agents (Explore, Plan, code-reviewer, etc.)
- Agents run autonomously with specific tool access
- Return results to parent agent
- Support for parallel agent execution

**Runix Code needs:**
```haskell
-- Subagent system (matches Claude Code's dispatch_agent)
data SubAgent m a where
  DispatchAgent :: AgentType -> AgentPrompt -> SubAgent m AgentResult

-- Subagent definition (loaded from .claude/agents/*.md or ~/.claude/agents/*.md)
data AgentDef = AgentDef
  { name :: Text
  , description :: Text
  , allowedTools :: Maybe [ToolName]  -- Nothing = inherit all
  , model :: Maybe ModelName           -- Nothing = inherit from parent
  , systemPrompt :: Text
  }

-- Built-in agents (from Claude Code)
data AgentType
  = PlanAgent          -- Built-in: uses Read, Glob, Grep, Bash for exploration
  | CustomAgent FilePath  -- User-defined from .claude/agents/

-- In Runix, agents are just functions with restricted effects:
planAgent :: Members '[LLM, FileSystem, Grep, Glob, Bash] r
          => PlanQuery -> Sem r Plan

customAgent :: Members effects r
            => AgentDef -> Query -> Sem r Result

-- Subagents run with separate context windows (no history pollution)
-- Tools can be restricted per agent
-- Effect constraints enforce security
```

**Implementation Details (from Claude Code):**
- Agents defined in Markdown with YAML frontmatter
- Project-level (`.claude/agents/`) overrides user-level (`~/.claude/agents/`)
- Separate context windows prevent main conversation pollution
- Can run up to 10 agents in parallel
- Each agent is stateless and isolated
- Tool restrictions enable least-privilege security

**Status:** ✅ Architecture supports this, need:
- [ ] Markdown+YAML parser for agent definitions
- [ ] Agent dispatch mechanism
- [ ] Separate context management per agent
- [ ] Built-in Plan agent implementation

---

#### 5. Web Capabilities
**Claude Code has:**
- `WebSearch` - Search and get results (US only, domain filtering)
- `WebFetch` - Fetch URL, convert HTML to markdown, AI summarization

**Runix Code needs:**
```haskell
data Web m a where
  -- Search the web
  WebSearch :: SearchQuery -> Web m [SearchResult]

  -- Fetch and process URL
  WebFetch :: URL -> WebFetch m WebContent

data SearchQuery = SearchQuery
  { query :: Text
  , allowedDomains :: [Domain]
  , blockedDomains :: [Domain]
  }

data SearchResult = SearchResult
  { title :: Text
  , url :: URL
  , snippet :: Text
  , source :: Text
  }

data WebContent = WebContent
  { url :: URL
  , title :: Text
  , markdown :: Text  -- Converted from HTML
  , redirectedTo :: Maybe URL
  }
```

**Implementation Details (from Claude Code):**

**WebSearch:**
- Server-side implementation by Anthropic (same as claude.com chat)
- Returns only title + URL for top results (lightweight)
- Supports domain filtering (allow-list/block-list)
- Can be rate-limited via `max_uses` parameter

**WebFetch:**
- Requires URL + prompt (question about the page)
- Never returns raw HTML/markdown - always answers the prompt
- Uses Claude 3.5 Haiku for summarization
- 15-minute cache, ~10MB limit, 100KB markdown after truncation
- HTML→Markdown via Turndown library
- Constrains verbatim quotes to 125 chars (copyright protection)

**For Runix Code:**
- WebSearch: Need search API (Brave/SerpAPI) or skip for MVP
- WebFetch: Use Haiku model for summarization (matches Claude Code)
- Implement 2-tool workflow: WebSearch finds URLs → WebFetch answers questions

**Status:** ⚠️ WebSearch needs API decision; WebFetch architecture is clear

---

#### 6. User Interaction
**Claude Code has:**
- `AskUserQuestion` - Multiple choice questions with 1-4 questions, 2-4 options each
- Multi-select support
- Automatic "Other" option for free-form text

**Runix Code needs:**
```haskell
data UserInteraction m a where
  AskQuestions :: [Question] -> UserInteraction m Answers

data Question = Question
  { question :: Text
  , header :: Text  -- Short label (max 12 chars)
  , options :: [Option]
  , multiSelect :: Bool
  }

data Option = Option
  { label :: Text
  , description :: Text
  }

type Answers = Map QuestionId [OptionLabel]
```

**Status:** ⚠️ Need TUI integration

---

#### 7. Observability & Debugging
**Claude Code has:**
- Status line showing current operation
- Todo list tracking (TodoWrite)
- System reminders
- Token budget tracking
- Error messages and warnings

**Runix Code needs:**
```haskell
data Observability m a where
  -- Status updates
  UpdateStatus :: Text -> Observability m ()

  -- Todo tracking
  UpdateTodos :: [Todo] -> Observability m ()

  -- System messages
  SystemReminder :: Text -> Observability m ()

  -- Resource tracking
  RecordTokenUsage :: Int -> Int -> Observability m ()  -- used, total

  -- Tracing
  TraceToolCall :: ToolName -> Input -> Observability m ()
  TraceLLMQuery :: Provider -> TokenCount -> Cost -> Observability m ()

data Todo = Todo
  { content :: Text
  , activeForm :: Text  -- Present continuous
  , status :: TodoStatus
  }

data TodoStatus = Pending | InProgress | Completed
```

**Status:** ⚠️ Need implementation + TUI display

---

### Advanced Features (Post-MVP)

#### 8. Code Intelligence
**Claude Code might have:**
- LSP integration for go-to-definition, type info
- Syntax highlighting in display
- Code completion suggestions

**Runix Code could add:**
```haskell
data CodeIntel m a where
  GetTypeInfo :: FilePath -> Position -> CodeIntel m TypeInfo
  FindDefinition :: FilePath -> Symbol -> CodeIntel m Location
  FindReferences :: FilePath -> Symbol -> CodeIntel m [Location]
  GetCompletions :: FilePath -> Position -> CodeIntel m [Completion]
```

**Status:** 🔮 Future work, complex

---

#### 9. Notebook Support
**Claude Code has:**
- Read Jupyter notebooks with cells and outputs
- Edit individual cells
- Execute notebooks

**Runix Code needs:**
```haskell
data Notebook m a where
  ReadNotebook :: FilePath -> Notebook m NotebookContent
  EditCell :: FilePath -> CellId -> CellSource -> Notebook m ()
  InsertCell :: FilePath -> CellId -> CellType -> CellSource -> Notebook m ()
  DeleteCell :: FilePath -> CellId -> Notebook m ()
```

**Status:** ⚠️ Lower priority for MVP

---

## Architecture

### Effect Stack

```haskell
-- Core effects for Runix Code agent
type RunixCodeEffects =
  '[ LLM provider model      -- LLM queries with tool calling
   , FileSystem              -- Read, Write, Edit, Glob, Grep
   , Shell                   -- Bash execution
   , Git                     -- Version control
   , Web                     -- Search, Fetch
   , UserInteraction         -- Questions, confirmations
   , Observability           -- Status, todos, traces
   , CompileTask             -- Haskell compilation (unique to Runix!)
   , Test                    -- Test execution (unique to Runix!)
   , Logging                 -- Structured logs
   ]

-- Sub-agents get restricted effects
type ExplorerEffects =
  '[ LLM provider model
   , FileSystem
   , Grep
   , Glob
   , Logging
   ]  -- No Shell, Git, Web - safer exploration
```

### Agent Loop

```haskell
-- Main agent loop (inspired by Claude Code)
agentLoop :: Members RunixCodeEffects r
          => AgentConfig
          -> [Message]
          -> UserInput
          -> Sem r Response
agentLoop config history userInput = do
  -- 1. Add user message to history
  let newHistory = history ++ [userMessage userInput]

  -- 2. Query LLM with tools
  response <- queryLLMWithTools config newHistory

  -- 3. Process response
  case response of
    TextResponse text ->
      return $ Response text newHistory

    ToolCalls calls -> do
      -- 4. Execute tools
      updateStatus "Executing tools..."
      results <- mapM executeToolCall calls

      -- 5. Add results to history
      let historyWithResults = newHistory
            ++ [assistantMessage calls]
            ++ map toolResultMessage results

      -- 6. Continue loop (recursive)
      agentLoop config historyWithResults Continue

    Finished text ->
      return $ Response text (newHistory ++ [assistantMessage text])

-- Tool execution with effect interpretation
executeToolCall :: Members RunixCodeEffects r
                => ToolCall -> Sem r ToolResult
executeToolCall (ToolCall name params) = do
  traceToolCall name params
  case name of
    "read_file" ->
      ReadFile (params ! "path") Nothing
    "write_file" ->
      WriteFile (params ! "path") (params ! "content")
    "bash" ->
      RunCommand (params ! "command") (params !? "timeout")
    "grep" ->
      Grep (parseGrepParams params)
    -- ... etc
```

### TUI Architecture (Brick)

```haskell
-- Application state
data AppState = AppState
  { conversationHistory :: [Message]
  , currentStatus :: Text
  , todos :: [Todo]
  , inputBuffer :: Text
  , viewport :: ViewportState
  , scrollPos :: Int
  , tokenUsage :: (Int, Int)  -- used, total
  }

-- Main UI layout
-- ┌─────────────────────────────────────────┐
-- │  Conversation History (scrollable)      │
-- │  User: Help me implement X              │
-- │  Assistant: I'll help with that...      │
-- │  [Tool Call: read_file]                 │
-- │  [Tool Result: ...]                     │
-- │                                         │
-- ├─────────────────────────────────────────┤
-- │  Status: Reading files... (5/10)        │
-- │  Todos: [✓] Read code [→] Implement X   │
-- │  Tokens: 15000/200000                   │
-- ├─────────────────────────────────────────┤
-- │  > User input here_                     │
-- └─────────────────────────────────────────┘

-- Brick app definition
app :: App AppState Event Name
app = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const theMap
  }

-- Event handling
handleEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleEvent (VtyEvent e) = case e of
  EvKey KEnter [] -> do
    input <- gets inputBuffer
    sendToAgent input
    modify $ \s -> s { inputBuffer = "" }

  EvKey (KChar 'c') [MCtrl] ->
    halt  -- Ctrl-C to exit

  EvKey (KChar c) [] ->
    modify $ \s -> s { inputBuffer = inputBuffer s <> T.singleton c }

  -- ... scrolling, etc.
```

**Questions:**
- Should we support multiple conversation sessions?
- Should we have vim-style keybindings?
- Should status/todos be collapsible?

---

## System Prompt Strategy

### Starting Point: Claude Code's Prompt

We'll start with a reverse-engineered version of Claude Code's system prompt based on observed behavior:

```markdown
# Runix Code System Prompt (v0.1)

You are Runix Code, an AI coding assistant built on the Runix framework.

## Core Principles
- Prioritize technical accuracy and truthfulness
- Be concise and professional
- Use tools frequently and in parallel when possible
- Always use specialized tools instead of bash for file operations
- Verify your work by reading files after writing

## Tool Usage
- When exploring codebases, use the Explore agent (via Task tool)
- Use Grep for content search, NOT bash grep
- Use Read for reading files, NOT cat
- Use Edit for modifying files, NOT sed
- Use Write only for new files, prefer Edit for existing files
- Run multiple independent tool calls in parallel

## Task Management
- Use TodoWrite proactively for complex multi-step tasks
- Keep one task in_progress at a time
- Mark tasks completed immediately when done
- Only mark completed when fully successful

## Git Workflows
- Never update git config
- Never run destructive operations without user request
- Only commit when explicitly asked
- Follow git safety protocol

## Communication
- Output text to communicate, not bash echo
- Use markdown formatting
- Include file:line references when discussing code
- Be objective, avoid excessive praise

You have access to the following tools:
[Tool definitions injected here]
```

### Customization for Runix

We'll add Runix-specific instructions:

```markdown
## Runix-Specific Features

### Haskell Development
- You can compile Haskell code using CompileTask
- Always compile and test before claiming success
- Use property-based testing with QuickCheck
- Follow Runix naming conventions (see NAMINGSCHEME.md)

### Effect System
- Tools are provided via Polysemy effects
- Sub-agents have restricted effect sets
- You cannot bypass effect constraints

### Self-Improvement (Advanced)
- You can generate new tools by writing Haskell code
- All generated tools must compile and pass tests
- Use the Tool Builder agent for this workflow
- Never modify the tool registry directly via LLM
```

---

## Implementation Plan (Revised)

### Architecture Principles

**Zero Coupling Between Layers:**
```
UI Layer (swappable) → Agent Core → Tools → Effects → Interpreters
```

- **Agent = Pure logic** (or effect-constrained via Polysemy)
- **Tools = Functions in `Sem r`** (universal-llm handles tool calling)
- **UI = Dumb shell** (collect input, call agent, display output)
- **No coupling** except via effects

**Error Handling Strategy:**
- Tool failures → Return as `ToolResult` (LLM handles it)
- Invalid tool calls → universal-llm returns error as `ToolResult`
- User interrupts → Exit immediately for MVP
- Unrecoverable errors → Let it crash

**State Management:**
- Main agent: Just `[Message]` history
- Subagents: Separate `[Message]` history (no shared state)
- Serializable for save/resume (JSON file, deferred to v0.2)

### Phase 1: Core Agent Loop (Week 1)

**Goal:** Working agent with basic tools, one-shot CLI

**Agent Core:**
- [ ] Core agent loop (`runAgent :: Sem r AgentResponse`)
- [ ] Tool dispatch via universal-llm
- [ ] History management (`[Message]`)
- [ ] System prompt loading (from `prompts/system.md`)

**Tools (as functions in FileSystem effect):**
- [ ] `read_file` - Read with line offset/limit
- [ ] `write_file` - Create new file
- [ ] `edit_file` - String replacement (old → new)
- [ ] `glob` - Pattern matching
- [ ] `grep` - Content search

**Tools (other effects):**
- [ ] `bash` - Shell execution (existing effect)
- [ ] `todo_write` - Todo tracking

**UI:**
- [ ] One-shot CLI (`runix-code "prompt"` → response → exit)
- [ ] Zero coupling to agent core

**Skip for v0.1:**
- BatchTool (implement tool, execute sequentially)
- Session persistence (design for it, implement later)
- Subagents (v0.2)
- WebFetch (v0.2)
- Advanced UI (REPL/Brick in v0.2)

### Phase 2: Polish & REPL (Week 2)

**Goal:** Interactive REPL or simple Brick UI

- [ ] REPL mode (loop: input → agent → output)
- [ ] OR minimal Brick TUI (viewport + input + status)
- [ ] Session save/load (JSON file)
- [ ] Better error messages
- [ ] Basic observability (status updates, token tracking)

### Phase 3: Subagents & BatchTool (Week 3)

**Goal:** Full Claude Code parity for core features

- [ ] Subagent dispatch (markdown + YAML definitions)
- [ ] Built-in Plan agent
- [ ] Separate context per subagent
- [ ] BatchTool parallel execution
- [ ] MultiEdit atomic operations

### Phase 4: Self-Improvement (Week 4-5)

**Goal:** The killer feature Claude Code can't have

- [ ] Test effect (run tests, check coverage)
- [ ] Tool Builder agent (fixed prompts)
- [ ] Tool registry (deterministic, not LLM-written)
- [ ] Compile + test + integrate workflow
- [ ] Agent generates tool → compiles → tests → adds to registry → restarts

### Phase 5: Advanced Features (Week 6+)

**Goal:** Production-ready, differentiated product

- [ ] WebFetch with Haiku summarization
- [ ] WebSearch (pluggable backends)
- [ ] Advanced observability (traces, metrics, cost tracking)
- [ ] Brick TUI polish (if not done in Phase 2)
- [ ] Performance optimizations
- [ ] Documentation and examples

---

## Actual Claude Code Tools (From Documentation)

Based on official documentation, Claude Code has these 11 tools:

1. **dispatch_agent** (Task tool) - Spawns subagents with restricted tool access
2. **Bash** - Shell execution with persistent sessions, timeout, background support
3. **BatchTool** - Parallel execution of multiple tool calls
4. **Read/ReadFile** - File reading with line offset/limit, image support
5. **Write** - File writing (overwrites existing)
6. **Replace** (Edit) - String-based file editing
7. **ReadNotebook** - Jupyter notebook reading
8. **NotebookEditCell** - Jupyter cell editing
9. **WebFetchTool** - URL fetching with AI summarization (Haiku)
10. **GlobTool** - File pattern matching
11. **GrepTool** - Content search (ripgrep-based)

**Missing tools we thought it had:**
- No separate WebSearch tool in CLI (only in API/web version)
- No explicit Git tool (uses Bash with special instructions)
- No AskUserQuestion (might be web-only)

**Additional tools Claude Code has that we didn't consider:**
- **BatchTool** - Parallel tool execution optimization
- **MultiEdit** - Atomic multi-edit to same file

---

## Architectural Decisions (Finalized)

### 1. Tool-to-Effect Mapping
**Decision:** Use `universal-llm`'s tool system directly.

- Tools are just functions in `Sem r`
- `ToolFunction` instance on return types for metadata
- Use `mkToolWithMeta` for custom parameter names
- Zero boilerplate for tool dispatch

**Example:**
```haskell
import Runix.LLM.ToolInstances ()  -- Enable Sem r support

-- Define result type
data FileContent = FileContent Text deriving (Show, Eq)

instance HasCodec FileContent where
  codec = named "FileContent" $ dimapCodec FileContent (\(FileContent t) -> t) codec

instance ToolFunction FileContent where
  toolFunctionName _ = "read_file"
  toolFunctionDescription _ = "Read a file from the filesystem"

-- Tool is just a function
readFileTool :: Members '[FileSystem] r => FilePath -> Sem r FileContent
readFileTool path = FileContent <$> readFile path

-- Use it
let tools = [LLMTool readFileTool]
```

### 2. FileSystem Extension Strategy
**Decision:** Add functions that run in FileSystem effect, not new effect.

- `editFile :: FilePath -> Text -> Text -> Bool -> Sem r ()`
- `multiEdit :: FilePath -> [(Text, Text)] -> Sem r ()`
- `applyDiff :: FilePath -> Diff -> Sem r ()`
- Keep as separate tools
- Move universally useful ones to Runix core later

**Why:** Simple, composable, no new effects needed.

### 3. Agent State Management
**Decision:** Main agent loop with `[Message]` history only.

```haskell
data AgentState = AgentState
  { history :: [Message]
  -- Future: todos, metrics, etc.
  }

runAgent :: Members effects r
         => AgentConfig
         -> AgentState
         -> UserInput
         -> Sem r (AgentResponse, AgentState)
```

- Serializable via `ToJSON`/`FromJSON`
- Subagents get separate `[Message]` history
- No shared state unless explicitly passed

### 4. Subagent Context Isolation
**Decision:** Separate history per subagent, return result only.

```haskell
runSubAgent :: Members effects r
            => AgentDef       -- Config (from .claude/agents/*.md)
            -> Query          -- Input
            -> Sem r Result   -- Output only, no history

-- Main agent sees only the result
-- Subagent's conversation is isolated
```

**Why:** Prevents context pollution, keeps main conversation clean.

### 5. BatchTool Implementation
**Decision:** Implement tool for compatibility, execute sequentially for v0.1.

```haskell
batchTool :: Members effects r => [ToolCall] -> Sem r [ToolResult]
batchTool calls = mapM executeToolCall calls  -- Sequential for now

-- v0.2: Add parallel execution with dependency analysis
```

**Why:** Performance optimization, not core functionality. Add parallelism later.

### 6. UI Architecture
**Decision:** Completely decoupled, swappable UI layer.

```
┌──────────────────┐
│  main.hs         │  ← One-shot CLI for v0.1, REPL/Brick for v0.2
│  - getInput      │
│  - runAgent      │
│  - displayOutput │
└────────┬─────────┘
         │ Zero coupling
         ▼
┌──────────────────┐
│  Agent.hs        │  ← Core logic, no UI knowledge
│  runAgent        │
└──────────────────┘
```

**v0.1:** One-shot CLI (`runix-code "prompt"` → response → exit)
**v0.2:** REPL or minimal Brick TUI

### 7. System Prompt Management
**Decision:** External markdown files, no templating for v0.1.

```
apps/runix-code/prompts/
  system.md          -- Main system prompt
  tool-usage.md      -- Tool-specific instructions (optional)
```

Load at startup:
```haskell
systemPrompt <- readFile "prompts/system.md"
```

**v0.2:** Add templating with project context (directory tree, git status, etc.)

### 8. Provider/Model Selection
**Decision:** Code is polymorphic, hardcode 1-2 combinations in main for v0.1.

```haskell
-- Agent core is polymorphic
runAgent :: forall provider model r.
            (LLMProvider provider, LLMModel model)
         => ...

-- Main hardcodes specific provider/model
main :: IO ()
main = runAgentM @'Anthropic @'Sonnet ...
```

**Why:** Flexibility for future, simplicity for MVP.

### 9. Session Persistence
**Decision:** Design for serializability, implement in v0.2.

```haskell
-- AgentState is serializable
instance ToJSON AgentState
instance FromJSON AgentState

-- v0.2: Add save/load
saveSession :: FilePath -> AgentState -> IO ()
loadSession :: FilePath -> IO AgentState
```

**Why:** Not critical for MVP, but easy to add later.

### 10. Error Handling
**Decision:** Fail-fast, return errors as ToolResults.

```haskell
-- Tool errors → ToolResult with error message
executeToolCall call = do
  result <- dispatchTool call  -- May fail
  return result  -- ToolResult includes success or error

-- LLM sees error, decides what to do
-- Invalid tool calls → universal-llm handles via JSON decode failures
-- Ctrl-C → Exit immediately (no graceful shutdown in v0.1)
-- Unrecoverable errors → Crash
```

**Philosophy:** Errors are data, not exceptions (for tools). LLM handles retries.

---

## Open Questions

### 1. Web Search Backend
**Finding:** Claude Code's CLI doesn't have WebSearch - it's server-side only (API/web version).

**Options for Runix Code:**
- **Skip for MVP** - Focus on local development tasks first
- **Brave Search API**: $3/1K searches, privacy-focused, good for AI
- **SerpAPI**: $50/mo for 5K searches, comprehensive
- **Make it pluggable** - Support multiple backends via effect interpreters

**Recommendation:** Skip for MVP, add later with pluggable backend.

### 2. WebFetch Implementation
**Finding:** Claude Code uses Haiku for summarization, never returns raw content.

**Decision:** Match Claude Code behavior:
- Use Haiku (or configurable fast model)
- Always require a prompt/question
- Return answer only, not raw markdown
- Implement HTML→Markdown conversion (use `pandoc` or similar)

### 3. Multi-Model Support
Should we support running different models for different tasks?
```haskell
-- Main agent uses Sonnet
mainAgent :: Members '[LLM 'Anthropic 'Sonnet, ...] r => Sem r ()

-- Web summarization uses Haiku
summarizeWeb :: Members '[LLM 'Anthropic 'Haiku, ...] r => Sem r Text
```

### 4. TUI Features
- Multiple session tabs?
- Vim keybindings?
- Mouse support?
- Syntax highlighting in output?
- Image display in terminal (kitty/iterm2 protocols)?

### 5. Session Management
- Auto-save on every message?
- Named sessions?
- Session search/browsing?
- Export conversation to markdown?

---

## Success Metrics

### Feature Parity
- [ ] Can perform all file operations Claude Code can
- [ ] Can execute shell commands with same safety
- [ ] Can use git workflows (commit, PR)
- [ ] Can spawn sub-agents for complex tasks
- [ ] Has comparable observability (status, todos)

### Differentiators
- [ ] Can compile and test Haskell code
- [ ] Can generate and integrate new tools
- [ ] Provides type-safe effect constraints
- [ ] Supports local LLM providers
- [ ] Has deterministic tool registration

### UX Quality
- [ ] Response time comparable to Claude Code
- [ ] TUI is intuitive and responsive
- [ ] Error messages are clear and actionable
- [ ] Conversation history is easy to navigate
- [ ] Session persistence works reliably

---

## Future Enhancements (Post-MVP)

### Advanced Agent Capabilities
- Multi-agent collaboration (agents calling agents)
- Long-running background tasks
- Scheduled/cron-like execution
- Agent performance benchmarking

### Developer Experience
- Plugin system for custom tools
- Custom agent templates
- Prompt engineering UI
- Agent behavior debugging tools

### Integration
- IDE plugins (VSCode, Neovim)
- CI/CD integration
- Slack/Discord bots
- API server mode

### Self-Improvement
- Automatic bottleneck detection
- A/B testing for improvements
- Tool usage analytics
- Performance regression detection

---

## Technical Debt / Risks

### Risks
1. **Web search costs**: Need to budget for API calls
2. **LLM costs**: Running agents can be expensive
3. **Haskell learning curve**: Users need to understand effects for customization
4. **Prompt drift**: If we diverge too much from Claude's prompts, quality may suffer
5. **Self-improvement safety**: Need robust testing to prevent agent from breaking itself

### Mitigation Strategies
1. Make web search optional, cache aggressively
2. Add cost tracking and budgets at effect level
3. Provide good examples and documentation
4. Version control prompts, A/B test changes
5. Require 80%+ test coverage, use property tests, git rollback on failure

---

## Appendix: Tool Definitions

### File Operations

#### read_file
```json
{
  "name": "read_file",
  "description": "Read a file from the filesystem",
  "parameters": {
    "file_path": {"type": "string", "required": true},
    "offset": {"type": "integer", "required": false},
    "limit": {"type": "integer", "required": false}
  }
}
```

#### write_file
```json
{
  "name": "write_file",
  "description": "Write a new file (errors if file exists without prior read)",
  "parameters": {
    "file_path": {"type": "string", "required": true},
    "content": {"type": "string", "required": true}
  }
}
```

#### edit_file
```json
{
  "name": "edit_file",
  "description": "Edit existing file via exact string replacement",
  "parameters": {
    "file_path": {"type": "string", "required": true},
    "old_string": {"type": "string", "required": true},
    "new_string": {"type": "string", "required": true},
    "replace_all": {"type": "boolean", "required": false, "default": false}
  }
}
```

#### glob
```json
{
  "name": "glob",
  "description": "Find files matching a pattern",
  "parameters": {
    "pattern": {"type": "string", "required": true},
    "path": {"type": "string", "required": false}
  }
}
```

#### grep
```json
{
  "name": "grep",
  "description": "Search file contents with regex",
  "parameters": {
    "pattern": {"type": "string", "required": true},
    "path": {"type": "string", "required": false},
    "case_insensitive": {"type": "boolean", "required": false},
    "output_mode": {"type": "string", "enum": ["content", "files_with_matches", "count"]},
    "context_before": {"type": "integer", "required": false},
    "context_after": {"type": "integer", "required": false},
    "show_line_numbers": {"type": "boolean", "required": false},
    "glob_filter": {"type": "string", "required": false},
    "multiline": {"type": "boolean", "required": false}
  }
}
```

### Shell Operations

#### bash
```json
{
  "name": "bash",
  "description": "Execute a bash command",
  "parameters": {
    "command": {"type": "string", "required": true},
    "description": {"type": "string", "required": false},
    "timeout": {"type": "integer", "required": false},
    "run_in_background": {"type": "boolean", "required": false}
  }
}
```

### Git Operations

#### git_commit
```json
{
  "name": "git_commit",
  "description": "Create a git commit",
  "parameters": {
    "message": {"type": "string", "required": true},
    "files": {"type": "array", "items": {"type": "string"}, "required": false}
  }
}
```

### Web Operations

#### web_search
```json
{
  "name": "web_search",
  "description": "Search the web",
  "parameters": {
    "query": {"type": "string", "required": true},
    "allowed_domains": {"type": "array", "items": {"type": "string"}},
    "blocked_domains": {"type": "array", "items": {"type": "string"}}
  }
}
```

#### web_fetch
```json
{
  "name": "web_fetch",
  "description": "Fetch and process a URL",
  "parameters": {
    "url": {"type": "string", "required": true},
    "prompt": {"type": "string", "required": true}
  }
}
```

### Observability

#### todo_write
```json
{
  "name": "todo_write",
  "description": "Update the todo list",
  "parameters": {
    "todos": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "content": {"type": "string"},
          "activeForm": {"type": "string"},
          "status": {"type": "string", "enum": ["pending", "in_progress", "completed"]}
        }
      }
    }
  }
}
```

---

## Key Findings from Claude Code Research

### What We Got Right
✅ File operations (Read, Write, Edit/Replace, Glob, Grep)
✅ Shell execution with timeout and background support
✅ LLM integration with tool calling
✅ Subagent/Task system with restricted access
✅ Todo tracking and observability
✅ Notebook support

### What We Missed
⚠️ **BatchTool** - Parallel tool execution optimizer (important for performance!)
⚠️ **MultiEdit** - Atomic multi-edit to same file
⚠️ WebSearch is server-side only, not in CLI
⚠️ Git is handled via Bash with system prompt instructions, not dedicated tool
⚠️ AskUserQuestion might be web-only

### Key Implementation Details Discovered

**System Prompt Philosophy:**
- Concise responses (< 4 lines unless detail requested)
- Minimize token output
- No emojis unless requested
- Refuse malicious code assistance
- Never use bash for file operations (use dedicated tools)
- Run tools in parallel when possible
- Use batch operations for git workflows

**Tool Design Patterns:**
- Tools return minimal data (WebSearch: just title+URL)
- Processing happens in separate AI calls (WebFetch uses Haiku)
- Caching for expensive operations (15min for WebFetch)
- Security through validation layers (domain checks, URL normalization)

**Subagent Architecture:**
- Markdown+YAML for agent definitions (not code!)
- Separate context windows (prevent pollution)
- Tool restrictions for security
- Stateless and parallelizable
- Up to 10 concurrent agents

**Git Workflows:**
- Handled via Bash tool, not separate effect
- System prompt provides git-specific instructions
- Batch commands in parallel (status + diff + log)
- HEREDOC for commit messages with footer
- Never use interactive mode (-i flag)

### Recommendations for Runix Code

**Priority 1 (MVP):**
1. Implement BatchTool equivalent (parallel execution)
2. Match Claude Code's concise communication style
3. Skip WebSearch for MVP (focus on local dev)
4. Use Bash for git (follow Claude Code pattern)
5. Implement Edit (Replace) for file modifications

**Priority 2 (Post-MVP):**
1. Add MultiEdit for atomic multi-file edits
2. Implement WebFetch with Haiku summarization
3. Add WebSearch with pluggable backends
4. Consider AskUserQuestion for TUI

**Unique Runix Additions:**
1. CompileTask effect (verify Haskell code)
2. Test effect (run tests, check coverage)
3. Effect-based security (compile-time guarantees)
4. Self-improvement via tool generation

---

## Revision History

- **2025-01-04**: Initial design document created
- **2025-01-04**: Updated with official Claude Code documentation findings
  - Added actual tool list (11 tools)
  - Clarified web tool architecture (WebSearch vs WebFetch)
  - Added subagent implementation details
  - Updated recommendations based on findings
  - Identified BatchTool and MultiEdit as missing features
- **2025-01-04**: Finalized architectural decisions
  - Documented zero-coupling UI architecture
  - Clarified tool-to-effect mapping via universal-llm
  - Defined error handling strategy (errors as data)
  - Specified state management (just `[Message]` history)
  - Updated implementation plan (5 phases, v0.1 = one-shot CLI)
  - Added 10 architectural decision rationales
  - Removed drift between discussion and documentation
- *Future revisions will be tracked here*
