use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::tempdir;

#[test]
fn narcissus_run_quickstart() {
    let mut cmd = Command::cargo_bin("narcissus").expect("binary exists");
    cmd.arg("run").arg("examples/quickstart.ns");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Hello from Narcissus!"));
}

#[test]
fn narcissus_eval_snippet() {
    let mut cmd = Command::cargo_bin("narcissus").expect("binary exists");
    cmd.arg("eval").arg("1 + 2 + 3");
    cmd.assert().success();
}

#[test]
fn orchid_init_creates_scaffold() {
    let dir = tempdir().expect("create temp dir");
    let project_path = dir.path().join("sample");

    let mut cmd = Command::cargo_bin("orchid").expect("binary exists");
    cmd.arg("init").arg(&project_path);
    cmd.assert().success();

    let script_path = project_path.join("src/main.ns");
    assert!(
        script_path.exists(),
        "orchid init should create src/main.ns"
    );
    let contents = fs::read_to_string(&script_path).expect("read scaffolded script");
    assert!(
        contents.contains("Hello from Narcissus"),
        "scaffold should contain greeting"
    );

    assert!(
        project_path.join("tests").exists(),
        "tests/ directory should exist"
    );
}
