use std::{
    fs, io,
    path::{Component, Path, PathBuf},
};

use clap::{Parser, Subcommand};
use narcissus::{
    Interpreter, NarcissusError,
    diagnostics::{Diagnostic, DiagnosticKind},
};

#[derive(Parser)]
#[command(author, version, about = "Orchid project tool for Narcissus")]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Scaffold a new Narcissus project (creates src/, tests/ skeleton)
    Init {
        #[arg(default_value = ".")]
        path: PathBuf,
    },
    /// Run the primary script for this project (defaults to src/main.ns)
    Run {
        #[arg(default_value = "src/main.ns")]
        script: PathBuf,
    },
    /// Execute *.ns scripts found under the tests/ directory
    Test,
    /// Add a dependency to the current project (placeholder)
    Add { name: String },
    /// Generate documentation (placeholder)
    Doc,
}

fn main() -> Result<(), NarcissusError> {
    let args = Args::parse();
    match args.command {
        Command::Init { path } => init_project(path)?,
        Command::Run { script } => run_script(&script)?,
        Command::Test => run_tests()?,
        Command::Add { name } => {
            println!("Dependency management is not implemented yet. Requested `{name}`.");
        }
        Command::Doc => {
            println!("Documentation generation is forthcoming. Stay tuned!");
        }
    }
    Ok(())
}

fn init_project(path: PathBuf) -> Result<(), NarcissusError> {
    let src_dir = path.join("src");
    let tests_dir = path.join("tests");
    let examples_dir = path.join("examples");

    fs::create_dir_all(&src_dir)?;
    fs::create_dir_all(&tests_dir)?;
    fs::create_dir_all(&examples_dir)?;

    let main_script = src_dir.join("main.ns");
    if !main_script.exists() {
        fs::write(
            &main_script,
            "// Narcissus entry point\nprintln(\"Hello from Narcissus!\")\n",
        )?;
    }

    let sample_test = tests_dir.join("smoke_test.ns");
    if !sample_test.exists() {
        fs::write(
            &sample_test,
            "// Example test script\nprintln(\"Running smoke test\")\n",
        )?;
    }

    let sample_example = examples_dir.join("hello.ns");
    if !sample_example.exists() {
        fs::write(&sample_example, "println(\"Hello from examples!\")\n")?;
    }

    println!("Initialized Narcissus workspace at {}", path.display());
    Ok(())
}

fn run_script(script: &Path) -> Result<(), NarcissusError> {
    let script = script
        .canonicalize()
        .map_err(|err| io_error("orchid run", script, err))?;
    let mut interpreter = Interpreter::new();

    let src_dir = script.parent().and_then(|dir| dir.canonicalize().ok());
    if let Some(ref canonical_src) = src_dir {
        load_project_sources(&mut interpreter, canonical_src, Some(&script))?;
    }

    let source = fs::read_to_string(&script).map_err(|err| io_error("orchid run", &script, err))?;
    let prefix = src_dir
        .as_ref()
        .map(|root| module_prefix(root, &script))
        .unwrap_or_default();
    interpreter.eval_source_with_prefix(&source, &prefix)?;
    Ok(())
}

fn run_tests() -> Result<(), NarcissusError> {
    let tests_dir = Path::new("tests");
    if !tests_dir.exists() {
        println!("No tests/ directory found. Skipping.");
        return Ok(());
    }

    let scripts = collect_ns_files(tests_dir)?;
    if scripts.is_empty() {
        println!("No .ns test files found under tests/.");
        return Ok(());
    }

    for script in &scripts {
        let mut interpreter = Interpreter::new();
        if let Some(src_dir) = Path::new("src").canonicalize().ok() {
            load_project_sources(&mut interpreter, &src_dir, None)?;
        }

        let source =
            fs::read_to_string(script).map_err(|err| io_error("orchid test", script, err))?;
        interpreter.eval_source(&source).map_err(|err| {
            eprintln!("Test {} failed: {err}", script.display());
            err
        })?;
    }

    println!("Executed {} test script(s).", scripts.len());
    Ok(())
}

fn collect_ns_files(root: &Path) -> Result<Vec<PathBuf>, NarcissusError> {
    let mut files = Vec::new();
    for entry in fs::read_dir(root)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            files.extend(collect_ns_files(&path)?);
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("ns") {
            files.push(path);
        }
    }
    files.sort();
    Ok(files)
}

fn load_project_sources(
    interpreter: &mut Interpreter,
    src_dir: &Path,
    skip: Option<&Path>,
) -> Result<(), NarcissusError> {
    let canonical_src = src_dir
        .canonicalize()
        .map_err(|err| io_error("orchid load", src_dir, err))?;
    if canonical_src.file_name().and_then(|s| s.to_str()) != Some("src") {
        return Ok(());
    }
    let files = collect_ns_files(&canonical_src)?;
    for file in files {
        if skip.map(|s| s == file).unwrap_or(false) {
            continue;
        }
        let source =
            fs::read_to_string(&file).map_err(|err| io_error("orchid load", &file, err))?;
        let prefix = module_prefix(&canonical_src, &file);
        interpreter.eval_source_with_prefix(&source, &prefix)?;
    }
    Ok(())
}

fn module_prefix(src_root: &Path, file: &Path) -> Vec<String> {
    let relative = file.strip_prefix(src_root).unwrap_or(file);
    let mut segments = Vec::new();
    if let Some(parent) = relative.parent() {
        for component in parent.components() {
            if let Component::Normal(part) = component {
                if let Some(text) = part.to_str() {
                    segments.push(text.to_string());
                }
            }
        }
    }
    segments
}

fn io_error(context: &str, path: &Path, err: io::Error) -> NarcissusError {
    let mut diagnostic = Diagnostic::new(
        DiagnosticKind::Runtime,
        format!("`{context}` failed for `{}`: {err}", path.display()),
    );
    if let Some(code) = err.raw_os_error() {
        diagnostic = diagnostic.with_note(format!("os error code: {code}"));
    }
    NarcissusError::from(diagnostic)
}
