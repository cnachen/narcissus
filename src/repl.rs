use rustyline::{DefaultEditor, error::ReadlineError};

use crate::{
    diagnostics::{NarcissusError, Result},
    runtime::Interpreter,
};

pub struct Repl {
    interpreter: Interpreter,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            interpreter: Interpreter::new(),
        }
    }

    pub fn run(&mut self) -> Result<()> {
        let mut editor = DefaultEditor::new().map_err(|err| {
            NarcissusError::from(std::io::Error::new(std::io::ErrorKind::Other, err))
        })?;
        loop {
            match editor.readline(">> ") {
                Ok(line) => {
                    let trimmed = line.trim();
                    if trimmed == ":quit" || trimmed == ":exit" {
                        break;
                    }
                    if trimmed.is_empty() {
                        continue;
                    }
                    editor.add_history_entry(trimmed).ok();
                    match self.interpreter.eval_source(trimmed) {
                        Ok(value) => {
                            println!("{value}");
                        }
                        Err(NarcissusError::Diagnostic(diag)) => {
                            eprintln!("{:?}: {}", diag.kind, diag.message);
                        }
                        Err(other) => eprintln!("error: {other}"),
                    }
                }
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
                Err(err) => {
                    return Err(NarcissusError::from(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        err,
                    )));
                }
            }
        }
        Ok(())
    }
}
