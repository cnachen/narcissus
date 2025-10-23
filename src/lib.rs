//! Core library for the Narcissus scripting language runtime and tooling.
//! Implements lexing, parsing, evaluation, and REPL utilities guided by
//! `DESIGNS.md`.

pub mod ast;
pub mod diagnostics;
pub mod environment;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod runtime;
pub mod stdlib;
pub mod value;

pub use diagnostics::{Diagnostic, DiagnosticKind, NarcissusError, SourceSpan};
pub use repl::Repl;
pub use runtime::{ExecutionContext, Interpreter};
