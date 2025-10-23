use std::fmt;

use thiserror::Error;

/// Represents a byte span within a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

impl SourceSpan {
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

/// Classification of a diagnostic event.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagnosticKind {
    Lexer,
    Parser,
    Runtime,
}

/// Rich diagnostic information surfaced to end users.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub message: String,
    pub span: Option<SourceSpan>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
            span: None,
            notes: Vec::new(),
        }
    }

    pub fn with_span(mut self, span: SourceSpan) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}", self.kind, self.message)?;
        if let Some(span) = self.span {
            write!(f, " ({}..{})", span.start, span.end)?;
        }
        if !self.notes.is_empty() {
            write!(f, "\n")?;
            for note in &self.notes {
                writeln!(f, "  note: {note}")?;
            }
        }
        Ok(())
    }
}

impl std::error::Error for Diagnostic {}

/// Unified error type for the Narcissus toolchain.
#[derive(Debug, Error)]
pub enum NarcissusError {
    #[error("{0}")]
    Diagnostic(#[from] Diagnostic),
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, NarcissusError>;
