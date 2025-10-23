# Narcissus Roadmap

This document tracks the features that are implemented today and outlines the milestones planned for future releases. The roadmap is split into three tiers:

- **Available** – ready for use in the current codebase
- **In Progress** – actively being implemented
- **Planned** – high-priority features not yet in development

## Available Today

### Language Core
- Kotlin-inspired syntax with brace-delimited blocks and optional semicolons
- `var` / `const` bindings with optional type annotations
- Functions (`fn`) with inferred return types
- Control flow: `if` / `else`, `when`, `while`, `loop`, `for … in`
- Arrays, maps, tuples, string literals, numeric literals
- Pattern matching via `when` arms (literal, identifier, tuple patterns)
- `break value`, `continue`, and `return` support
- Gradual typing surface (annotations parsed but semantics evolving)

### Runtime & Interpreter
- AST interpreter with expression/block evaluation
- Copy-on-write semantics for arrays and maps; shared values by default
- In-place updates for array indices and map fields
- Environment scoping with lexical modules and closures
- Native diagnostic system for parser/lexer/runtime errors

### Standard Library
- `std.io`: `print`, `println`, `dbg`, `read_line`
- `std.string`: length, emptiness check, casing helpers, trim, split, replace, starts/ends-with, join
- `std.collections`: length, push, insert, keys, values, `range`, `range_step`, `pop`
- `std.math`: abs, floor, ceil, sqrt, round, pow
- `std.time`: `now` (UNIX timestamp)
- `std.runtime`: `sleep`
- `std.fs`: `read_text`, `write_text`, `exists`, `create_dir_all`, `remove`
- `std.net`: placeholder (returns diagnostic)
- Prelude exports: `print`, `println`, `dbg`, `len`

### Tooling
- `narcissus` CLI with `run`, `eval`, and `repl` subcommands
- `orchid` CLI with `init`, `run`, and `test` commands
- Examples under `examples/` for quickstart, collections, and pattern usage

### Testing & Quality
- Integration tests for interpreter semantics and stdlib helpers (`tests/interpreter.rs`)
- CLI smoke tests using `assert_cmd` (`tests/cli.rs`)
- Continuous formatting via `cargo fmt`

## In Progress / Near Term
- Broaden stdlib surface (iterator helpers, date/time formatting, richer math)
- Improve diagnostic reporting (multi-span notes, hints, suggestion engine)
- Extend `orchid` commands (dependency management, documentation generation)
- Harden filesystem/network operations (permissions handling, sandbox support)

## Planned (Mid-Term)

### Language & Runtime
- Module import system with `use` resolution and package lookup
- Async/await execution model with task scheduler
- Channels and structured concurrency primitives
- Optional static type checker with gradual typing enforcement
- Bytecode VM / JIT experimentation for performance improvements
- Sandboxed execution contexts for untrusted scripts

### Standard Library
- Networking (`std.net`) with HTTP client/server primitives
- `std.test` module for assertions, property testing, and harness support
- Rich collections API (iterators, filters, maps, sets)
- `std.process` for spawning child processes (with sandbox controls)
- Serialization helpers (JSON, YAML) with opt-in features

### Tooling
- `orchid add`, `orchid remove`, `orchid update` with dependency resolution
- Package manifest (`orchid.toml`) and lockfile management
- Project templates for libraries and binaries
- Formatter (`narcissus fmt`) and linter (`narcissus lint`) commands
- Language server protocol (LSP) integration for editor support

### Operations & Ecosystem
- Continuous integration pipelines (lint, test, multi-platform builds)
- Binary releases for major platforms (Linux/macOS/Windows)
- Documentation site with API reference and tutorials
- Community processes: contribution guide, code of conduct, issue triage policy

## Suggestions & Feedback

Roadmap priorities evolve with community input. To propose features or reprioritise tasks:

- Open an issue describing the motivation and proposed approach
- Start a design discussion referencing `docs/DESIGNS.md`
- Contribute pull requests with accompanying docs/tests where possible

Use this document as the canonical reference for project direction; update status markers as work progresses.
