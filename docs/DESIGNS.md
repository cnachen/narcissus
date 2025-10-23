# Narcissus Language Design

The project aims to create a scripting language called **Narcissus** (`.ns`). It balances approachability with a robust, opt-in type system and tooling inspired by modern languages (Rust, Lua, Kotlin, C#, TypeScript, Python). Development is test-driven; keep unit tests under `tests/` and evolve the language spec in `docs/`.

## Language Goals
- Embrace readability-first syntax with a small, orthogonal feature set.
- Run scripts quickly via an interpreter (`narcissus`) while enabling ahead-of-time compilation later.
- Support lightweight tooling (`orchid`) for dependency management, testing, running, and packaging.
- Separate core language semantics from the standard library; re-export ergonomic defaults through a prelude.

## Source Files and Modules
- Source files use UTF-8 text with `.ns` extension.
- One **module** per file by default; nested modules declared with `module foo.bar { ... }`.
- Imports mirror the module tree: `use math.trig.sin`.
- Each module can expose public symbols via `pub` modifiers; unmarked items are private to the module file.

## Lexical Structure
- Significant whitespace for indentation (4 spaces recommended), but braces remain mandatory.
- Single-line comments use `//`; block comments use `/* ... */` and can nest.
- Identifiers use Unicode letters and `_`, but ASCII is preferred; identifiers are case-sensitive.
- Numeric literals support underscores for readability (`1_000_000`), binary (`0b1010`), octal (`0o755`), hex (`0xFF`), and floating point (`3.14`, `2e-3`).

## Basic Syntax
- Variables declared with `var name: Type = expr`; type may be omitted if inferable.
- Constants declared with `const NAME: Type = expr`; constants are compile-time evaluated.
- Blocks `{ ... }` create new scopes; bindings default to mutable.
- Statements end implicitly at newline or explicitly with `;` when chaining.
- Control flow constructs:
  - `if` / `else if` / `else` expressions evaluate to a value.
  - `when` expression for pattern-style branching (`when value { 0 -> ..., else -> ... }`).
  - Loop forms: `while condition {}`, `loop {}` with `break value`, and `for item in iterable {}`.
- Functions declared with `fn name(params) -> ReturnType { ... }`; return type optional if inferrable or unit.
- Lambdas use Kotlin-like syntax: `fn transform = |x| x * 2`.

## Types and Type System
- Optional static typing: type annotations are default-off but recommended for public APIs.
- Gradual typing via `dyn` keyword to opt into dynamic dispatch (`var dyn_value: dyn Any`).
- Primitive types: `Int`, `Float`, `Bool`, `String`, `Char`, `Unit`.
- Compound types:
  - Tuples: `(Int, String)`
  - Arrays: `[T; N]` for fixed length, `Vec<T>` for growable sequences.
  - Maps: `Map<K, V>`; maintain insertion order.
  - Optional values: `Option<T>` with `some(value)` / `none`.
- User-defined types:
  - `struct` for product types with named fields.
  - `enum` for tagged sum types with pattern matching support.
  - `trait` for behavior contracts; structs/enums `impl` traits.
- Generics: `struct Box<T> { value: T }`; constraints via `where T: Trait`.
- Type inference uses local constraint solving; fails fast with actionable diagnostics.

## Memory Model
- Automatic memory management via a generational garbage collector tuned for short-lived values.
- Values are shared by default; mutations copy-on-write for composite data (arrays, maps, strings).
- No ownership or borrowing syntax; runtime enforces interior mutability rules on shared references.
- Native extensions can opt into manual resource control using scoped finalizers.

## Error Handling
- Checked errors via `Result<T, E>` with `try` keyword for early returns (`try do_work()`).
- Unrecoverable errors trigger `panic`, capturing a stack trace.
- Pattern matching on `Result` and `Option` is idiomatic.
- Exceptions are not part of the language; use `panic` only for bugs.

## Concurrency & Async
- Lightweight async/await syntax: `async fn fetch() -> Result<Response>`.
- `await` suspends within async blocks; executor provided by std `runtime` module.
- Channels (`channel::Sender`, `channel::Receiver`) for message passing; foster structured concurrency via task scopes.
- Data shared across tasks is cloned lazily; no thread-safety annotations required for pure Narcissus code.

## Standard Library & Prelude
- Core namespaces: `io`, `fs`, `math`, `string`, `collections`, `runtime`, `time`, `net`.
- Current coverage includes string helpers (`replace`, `split`, `starts_with`/`ends_with`, `join`), collection utilities (`range`, `range_step`, `pop`, `push`, `insert`), math helpers (`abs`, `floor`, `ceil`, `round`, `sqrt`, `pow`), timing (`time.now`, `time.sleep` / `runtime.sleep`), and filesystem utilities (`read_text`, `write_text`, `exists`, `create_dir_all`, `remove`).
- Prelude re-exports targeted helpers such as `print`, `println`, `dbg`, and `len`.
- Standard library shipped as versioned package; users can opt for minimal runtime by disabling default features.
- Interoperability: `ffi` module exposes C ABI bindings; conversions to/from Rust `serde` for serialization.

## Tooling
- `narcissus`: interpret scripts, run REPL (`narcissus repl`), compile to bytecode when backend ready.
- `orchid`: manage packages, run tests, generate docs:
  - `orchid init` scaffolds a project with `src/`, `tests/`, and `examples/` directories.
  - `orchid run [script]` executes a project script (defaults to `src/main.ns`).
  - `orchid test` executes `.ns` scripts discovered under `tests/`.
  - `orchid add <pkg>` resolves semantic versions from registry (planned).
- Build artifacts stored under `target/`; caches preserved between runs.

## Testing & Documentation
- Encourage behavior-driven tests: `test "handles empty array" { ... }`.
- Language ships with `std::test` utilities for assertions and property tests.
- Documentation comments `///` support Markdown; `orchid doc` renders HTML reference.
- Keep design docs and RFCs under `docs/` with incremental versioning.

## Implementation Notes
- Parser follows Pratt-style expressions to support extensible operator precedence.
- Bytecode VM phase 1; future phases plan for JIT/AOT optimization.
- Diagnostic system aligns with Rust-style structured errors (labels, notes, suggestions).
- Keep codebase readable and well-commented; prefer small modules with clear responsibilities.
