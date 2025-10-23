use std::{fs, path::PathBuf};

use clap::{Parser, Subcommand};

use narcissus::{Interpreter, NarcissusError, Repl};

#[derive(Parser)]
#[command(author, version, about = "Narcissus language interpreter")]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Run a Narcissus script file
    Run { script: PathBuf },
    /// Start an interactive REPL session
    Repl,
    /// Evaluate a snippet of Narcissus code
    Eval { source: String },
}

fn main() -> Result<(), NarcissusError> {
    let args = Args::parse();
    match args.command.unwrap_or(Command::Repl) {
        Command::Run { script } => run_script(script),
        Command::Repl => {
            let mut repl = Repl::new();
            repl.run()
        }
        Command::Eval { source } => {
            let mut interpreter = Interpreter::new();
            interpreter.eval_source(&source)?;
            Ok(())
        }
    }
}

fn run_script(path: PathBuf) -> Result<(), NarcissusError> {
    let source = fs::read_to_string(&path)?;
    let mut interpreter = Interpreter::new();
    interpreter.eval_source(&source)?;
    Ok(())
}
