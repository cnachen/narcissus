# Repository Guidelines

## The Narcissus Language Design in DESIGNS.md
- Keeps this file and `docs/` updated after the codebase changed.
- Do not touch files in `examples/`, it's gold.

## Project Structure & Module Organization
The workspace is a single Rust crate. Core language/runtime code lives in `src/lib.rs` and supporting modules under `src/`. Command-line front ends live in `src/bin/`:
- `src/bin/narcissus.rs`: interpreter CLI (`run`, `eval`, `repl`).
- `src/bin/orchid.rs`: project scaffolding/management stub.
Place integration and CLI tests in `tests/`; fixtures sit under `tests/fixtures/`. Temporary build artefacts appear in `target/` (never commit).

## Build, Test, and Development Commands
- `cargo check` performs fast type-checking and is ideal before committing.
- `cargo build --release` produces an optimized binary, stored under `target/release/`.
- `cargo run --bin narcissus -- <subcommand>` executes scripts (e.g. `cargo run --bin narcissus -- run examples/quickstart.ns`).
- `cargo run --bin orchid -- <subcommand>` exercises the project tooling (e.g. `cargo run --bin orchid -- init demo`).
- `cargo test` runs unit and integration tests; combine with `-- --nocapture` to print test output when debugging.
- `cargo clippy -- -D warnings` enforces lints; treat warnings as failures.
- `cargo fmt` formats the codebase using Rustfmt defaults.

## Coding Style & Naming Conventions
Follow idiomatic Rust 2024 style with 4-space indentation. Run `cargo fmt` before opening a PR to keep diffs clean. Prefer meaningful module names that mirror the folder structure and keep public APIs in `lib.rs`. Use `CamelCase` for types and traits, `snake_case` for functions, variables, and files, and `SCREAMING_SNAKE_CASE` for constants. Document non-trivial functions with `///` doc comments that explain behavior and invariants.

## Testing Guidelines
Add unit tests alongside implementation blocks using `#[cfg(test)]` modules when feasible, and broader scenarios under `tests/`. Existing suites include:
- `tests/interpreter.rs` for language/runtime coverage.
- `tests/cli.rs` for `narcissus`/`orchid` smoke checks (requires `assert_cmd`, `tempfile`).
Name tests after the behavior they verify (e.g., `array_element_assignment_updates_value`). When adding features, include regression coverage for edge cases and failure paths. Store data fixtures under `tests/fixtures/` and load via relative paths. Run `cargo test` (optionally with `-- --nocapture`) before pushing; ensure new tests pass in both debug and release if performance-sensitive code is touched.

## Commit & Pull Request Guidelines
Commits should be small, self-contained, and use imperative present tense (e.g., “Add parser for config”). Include context in the body when behavior or interfaces change. For pull requests, write a short summary of the problem and solution, list testing performed (`cargo test`, `cargo fmt`, etc.), and link issues when relevant. Attach terminal output or screenshots if your change affects runtime behavior to help reviewers reproduce results quickly.
