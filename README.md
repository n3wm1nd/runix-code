# Runix Code

An AI-powered coding assistant built on the Runix framework.

## What It Is

Runix Code is an AI coding assistant with a key difference: **every tool's capabilities are verified at compile time**. When a tool compiles with a specific type signature, you have a mathematical guarantee about what it can and cannot do.

This is possible because Runix Code is built on Runix's effect system (Polysemy), where tools declare their required capabilities:

```haskell
-- This tool can ONLY read/write files - the compiler guarantees it
readFile :: Members '[FileSystemRead, FileSystemWrite, Fail] r
         => FilePath -> Sem r ReadFileResult

-- This tool can ONLY execute shell commands
bash :: Member Bash r => Command -> Sem r BashResult
```

## Features

- **Type-safe tool calling** - Tools are Haskell functions with effect constraints
- **File operations** - Read, write, edit, glob, grep, diff
- **Shell execution** - Run commands with proper error handling
- **Build integration** - Compile Haskell projects and verify errors
- **Code generation** - Generate new tools with compile-time verification
- **Multi-model support** - Claude, OpenAI, local models, OpenRouter, ZAI
- **Interactive TUI** - Brick-based terminal interface
- **Session persistence** - Save and resume conversations
- **Claude Code integration** - Supports `.claude/` directory structure (agents, skills, CLAUDE.md)

## Quick Start

### Prerequisites

- GHC 9.10.3 or later
- Cabal 3.14 or later
- Dependencies: `runix`, `universal-llm`, `polysemy`, `brick` (see cabal file)
- An API key for your chosen LLM provider

### Running

```bash
# Set up environment variables
export ANTHROPIC_OAUTH_TOKEN="your-token"
export RUNIX_MODEL="claude-sonnet-45"  # Default if not set

# Run the TUI
cabal run runix-code-tui

# Or run the simple CLI
cabal run runix-code
```

### Model Configuration

Choose your LLM provider via the `RUNIX_MODEL` environment variable:

```bash
# Claude via Anthropic (default)
export RUNIX_MODEL="claude-sonnet-45"
export ANTHROPIC_OAUTH_TOKEN="your-token"

# Local models via llama.cpp
export RUNIX_MODEL="qwen3-coder"
export LLAMACPP_ENDPOINT="http://localhost:8080/v1"

# Any model via OpenRouter
export RUNIX_MODEL="openrouter"
export OPENROUTER_API_KEY="your-key"
export OPENROUTER_MODEL="anthropic/claude-3.5-sonnet"

# GLM models via ZAI
export RUNIX_MODEL="glm-46-zai"
export ZAI_API_KEY="your-key"
```

See [lib/Config.hs](lib/Config.hs) for all supported model strings and configuration options.

## Built-in Tools

The agent has access to these tools (each with compile-time constrained capabilities):

- **File Operations**: `read_file`, `write_file`, `edit_file`, `glob`, `grep`, `diff`
- **Shell**: `bash` - Execute shell commands
- **Build**: `cabal_build` - Compile and check for errors
- **User Input**: `ask` - Request input during execution
- **Task Tracking**: `todo_write`, `todo_read`, `todo_check`, `todo_delete`
- **Code Generation**: `generate_tool` - Create new tools (with compile verification)

### Tool Safety

Each tool's type signature proves what it can access:
- `read_file` and `write_file` can only access the filesystem
- `bash` can only execute shell commands
- `ask` can only interact with the user
- `grep` can only search file contents

The compiler enforces these constraints—there's no way for a tool to escape its declared effects.

## Architecture

Runix Code demonstrates how to build AI agents with Runix:

### Agent as Function

The core agent is just a function with declared effect requirements:

```haskell
runixCode :: ( Member (LLM model) r
             , Member Grep r
             , Member FileSystemRead r
             , Member FileSystemWrite r
             -- ... other effects
             )
          => SystemPrompt -> UserPrompt -> Sem r (RunixCodeResult model)
```

This means it can be:
- Called from other Runix tasks
- Used as a tool itself (via `ToolFunction` instance)
- Composed with other Polysemy effects

### Tools as Functions

Tools are ordinary Haskell functions that implement the `ToolFunction` typeclass:

```haskell
-- Define result type
newtype ReadFileResult = ReadFileResult Text

-- Implement ToolFunction for metadata
instance ToolFunction ReadFileResult where
  toolFunctionName _ = "read_file"
  toolFunctionDescription _ = "Read a file from the filesystem"

-- Implement the tool
readFile :: Members '[FileSystemRead, FileSystemWrite, Fail] r
         => FilePath -> Sem r ReadFileResult
```

The universal-llm library handles JSON schema generation and parameter dispatch automatically.

### Self-Improvement

The `generate_tool` capability allows the agent to:
1. Generate a new Haskell function with type signature
2. Compile it with `cabal build` to verify type safety
3. If successful, integrate the tool into the running system
4. If compilation fails, see errors and iterate

This is only safe because the compiler verifies effect constraints—generated code cannot escape its declared capabilities.

## Claude Code Integration

Runix Code supports the Claude Code `.claude/` directory structure:

- **`.claude/agents/*.md`** - Subagents with isolated contexts
- **`.claude/skills/*/SKILL.md`** - Skills with custom scripts
- **`CLAUDE.md`**, **`.claude/CLAUDE.md`** - Project instructions

These are automatically loaded and made available as tools at runtime.

## Building

```bash
# Build all targets
cabal build all

# Build specific target
cabal build runix-code-tui

# Run tests
cabal test

# Clean
cabal clean
```

## Project Structure

```
runix-code/
├── lib/              # Core agent library
│   ├── Agent.hs      # Main agent loop
│   ├── Tools.hs      # Tool implementations
│   ├── Tools/
│   │   └── Claude.hs # Claude Code integrations
│   ├── Config.hs     # Configuration and model selection
│   ├── Models.hs     # Model-specific configurations
│   └── Runner.hs     # Effect interpreters
├── tui/              # Brick-based TUI
│   ├── Main.hs
│   └── UI/           # UI components
├── cli/              # Simple CLI interface
│   └── Main.hs
├── test/             # Tests
├── bench/            # Benchmarks
└── prompt/           # System prompts
```

## What Makes This Different

Most AI coding assistants run in Python or JavaScript where any code can do anything. Runix Code runs in Haskell with:

- **Compile-time capability constraints** - The type system proves what each tool can do
- **Effect-based security** - Tools cannot escape their declared effects
- **Safe code generation** - LLM-generated tools are compiled and verified before execution

If a tool compiles with type signature `Members '[FileSystem] r => ...`, the compiler mathematically proves it cannot access network, execute commands, or do anything except file operations.

This level of compile-time safety is impossible in other languages.

## Documentation

- [DESIGN.md](DESIGN.md) - Architecture and design decisions

## Built With

- **Runix** - Effect-based task framework providing security and composability
- **Universal-LLM** - Type-safe multi-provider LLM library
- **Polysemy** - Effect system for Haskell
- **Brick** - Terminal UI library

## License

GPL3 - See [LICENSE](LICENSE)
