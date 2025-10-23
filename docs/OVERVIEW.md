# Narcissus Language Overview

Narcissus is a gradually typed scripting language that blends Kotlin-like syntax with Rust-inspired tooling. This overview highlights the language concepts, the available standard modules, and the tooling you can use today.

## Getting Started

### Installation

Build Narcissus from source:

```bash
cargo build --release
```

This produces CLI binaries under `target/release/`:

- `narcissus`: interpreter and REPL
- `orchid`: project scaffolding and management tool (preview)

### Hello World

Create `hello.ns`:

```text
print("Hello, Narcissus!")
```

Run it:

```bash
cargo run --bin narcissus -- run hello.ns
```

Start a REPL:

```bash
cargo run --bin narcissus -- repl
```

## Language Basics

### Variables and Constants

Use `var` for mutable bindings (mark with `mut` when you need reassignment) and `const` for compile-time constants:

```text
var mut counter = 0
const LIMIT = 10
```

Type annotations are optional:

```text
var name: String = "Ada"
```

### Functions

Declare functions with `fn`:

```text
fn add(a, b) {
    return a + b
}

print(add(2, 3))
```

Return types are inferred; `async fn` is reserved for upcoming async support.

### Control Flow

- `if` / `else if` / `else` expressions
- `when` expressions for pattern-style branching
- Loops: `while`, `loop`, and `for item in iterable`
- `break` and `continue`; `break value` yields the value from the loop

Example `when` expression:

```text
when value {
    0 -> { print("zero") }
    1 -> { print("one") }
    else -> { print("many") }
}
```

### Collections and Maps

Square brackets create arrays; curly braces with key/value pairs create maps:

```text
var mut numbers = [1, 2, 3]
var mut inventory = {
    "apples": 3,
    "bananas": 5,
}

numbers[1] = 42
inventory.bananas = inventory.bananas + 1
```

Arrays, strings, and maps can be iterated:

```text
for pair in std.collections.values(inventory) {
    print(pair)
}
```

## Standard Library Snapshot

The `std` namespace exposes modular utilities. Frequent helpers (`print`, `println`, `dbg`, `len`) are re-exported in the prelude.

### io

- `std.io.print(...)`, `std.io.println(...)`
- `std.io.read_line()` reads a line from stdin
- `std.io.dbg(value)` prints debug representation and returns the value

### string

- `std.string.len(text)`
- `std.string.is_empty(text)`
- `std.string.to_upper(text)`
- `std.string.trim(text)`
- `std.string.split(text, separator)`
- `std.string.replace(text, from, to)`
- `std.string.starts_with(text, prefix)`
- `std.string.ends_with(text, suffix)`
- `std.string.join(array_of_strings, separator)`

### collections

- `std.collections.len(value)` works for strings, arrays, maps
- `std.collections.push(array, item)` returns a new array with the item appended
- `std.collections.insert(map, key, value)` returns a new map
- `std.collections.keys(map)` and `std.collections.values(map)`
- `std.collections.range(start, end)` and `std.collections.range_step(start, end, step)`
- `std.collections.pop(array)` returns a map containing the popped value and updated array

### math

- `std.math.abs(number)`
- `std.math.floor(number)`
- `std.math.ceil(number)`
- `std.math.sqrt(number)` (expects non-negative input)
- `std.math.round(number)`
- `std.math.pow(base, exponent)`

### time

- `std.time.now()` returns the UNIX timestamp in seconds
- `std.time.sleep(milliseconds)` pauses execution (alias of `std.runtime.sleep`)

### runtime

- `std.runtime.sleep(milliseconds)` pauses execution

### fs

- `std.fs.read_text(path)` reads a UTF-8 file
- `std.fs.write_text(path, text)` writes a file
- `std.fs.exists(path)` checks file/directory presence
- `std.fs.create_dir_all(path)` creates directory hierarchies
- `std.fs.remove(path)` removes a file

### net

- Currently unimplemented; calling functions raises a diagnostic

## Tooling

### `narcissus` CLI

```
narcissus run <script.ns>
narcissus eval "expression"
narcissus repl
```

### `orchid` CLI (Preview)

```
orchid init [path]
orchid run [script]
orchid test
```

Initialises a project with a standard skeleton, runs scripts, and executes `.ns` files beneath `tests/`. Dependency management commands (e.g., `orchid add`) remain on the roadmap.

## Testing & Examples

- `tests/interpreter.rs` exercises language semantics and stdlib helpers
- `tests/cli.rs` uses `assert_cmd` to verify the CLI binaries

Run the entire suite:

```bash
cargo test
```

Scripts in `examples/` showcase typical usage:

- `quickstart.ns`: hello world and simple function
- `collections.ns`: map/array updates via std helpers
- `patterns.ns`: `when` expression demo
- `app/`: scaffolded multi-file project (`src/`, `tests/`, `examples/`) demonstrating stdlib usage

Execute an example:

```bash
cargo run --bin narcissus -- run examples/collections.ns
```

Run the scaffolded project with Orchid:

```bash
cargo run --bin orchid -- run examples/app/src/main.ns
```

## Additional References

- `docs/DESIGNS.md`: language design document
- `docs/ROADMAPS.md`: feature status and roadmap
- `AGENTS.md`: contributor guidelines
- `docs/OVERVIEW.md`: this quickstart (keep synced with roadmap updates)

For feature requests or contributions, follow the guidelines above and include documentation/tests with your changes.
