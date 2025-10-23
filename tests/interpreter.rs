use indexmap::IndexMap;
use narcissus::{
    diagnostics::NarcissusError,
    runtime::Interpreter,
    value::{Value, ValueKind},
};
use std::path::Path;
use tempfile::tempdir;

fn eval(source: &str) -> Value {
    let mut interpreter = Interpreter::new();
    interpreter
        .eval_source(source)
        .expect("evaluation should succeed")
}

fn eval_error(source: &str) -> NarcissusError {
    let mut interpreter = Interpreter::new();
    match interpreter.eval_source(source) {
        Ok(value) => panic!("expected error, received value {value}"),
        Err(err) => err,
    }
}

fn expect_int(value: &Value) -> i64 {
    match value.0.as_ref() {
        ValueKind::Int(n) => *n,
        _ => panic!("expected Int, found {}", value.type_name()),
    }
}

fn expect_map(value: &Value) -> &IndexMap<String, Value> {
    match value.0.as_ref() {
        ValueKind::Map(map) => map,
        _ => panic!("expected Map, found {}", value.type_name()),
    }
}

fn expect_bool(value: &Value) -> bool {
    match value.0.as_ref() {
        ValueKind::Bool(b) => *b,
        _ => panic!("expected Bool, found {}", value.type_name()),
    }
}

fn path_literal(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "\\\\")
}

#[test]
fn evaluates_basic_arithmetic() {
    let value = eval("return 2 + 2;");
    assert_eq!(expect_int(&value), 4);
}

#[test]
fn matches_when_pattern() {
    let value = eval(
        r#"
        var value = 2
        when value {
            1 -> { return 1; }
            2 -> { return 42; }
            else -> { return 0; }
        }
        "#,
    );
    assert_eq!(expect_int(&value), 42);
}

#[test]
fn returns_last_expression_from_script() {
    let value = eval(
        r#"
        var x = 40
        x + 2
        "#,
    );
    assert_eq!(expect_int(&value), 42);
}

#[test]
fn break_carries_value_out_of_loop() {
    let value = eval(
        r#"
        loop {
            break 7
        }
        "#,
    );
    assert_eq!(expect_int(&value), 7);
}

#[test]
fn for_loop_accumulates_sum() {
    let value = eval(
        r#"
        var mut sum = 0
        for item in [1, 2, 3, 4] {
            sum = sum + item
        }
        sum
        "#,
    );
    assert_eq!(expect_int(&value), 10);
}

#[test]
fn map_field_assignment_updates_value() {
    let value = eval(
        r#"
        var mut inventory = {
            "apples": 3,
            "bananas": 7
        }
        inventory.bananas = inventory.bananas + 5
        inventory
        "#,
    );
    let map = expect_map(&value);
    assert_eq!(map.len(), 2);
    assert_eq!(expect_int(map.get("apples").unwrap()), 3);
    assert_eq!(expect_int(map.get("bananas").unwrap()), 12);
}

#[test]
fn array_element_assignment_updates_value() {
    let value = eval(
        r#"
        var mut numbers = [1, 2, 3]
        numbers[1] = numbers[1] + 5
        numbers
        "#,
    );
    match value.0.as_ref() {
        ValueKind::Array(values) => {
            assert_eq!(values.len(), 3);
            assert_eq!(expect_int(&values[0]), 1);
            assert_eq!(expect_int(&values[1]), 7);
            assert_eq!(expect_int(&values[2]), 3);
        }
        _ => panic!("expected Array, found {}", value.type_name()),
    }
}

#[test]
fn recursive_function_evaluates() {
    let value = eval(
        r#"
        fn fib(n) {
            if n <= 1 {
                return n
            }
            return fib(n - 1) + fib(n - 2)
        }

        fib(6)
        "#,
    );
    assert_eq!(expect_int(&value), 8);
}

#[test]
fn const_assignment_is_rejected() {
    let err = eval_error(
        r#"
        const answer = 42
        answer = 13
        "#,
    );
    let message = format!("{err}");
    assert!(
        message.contains("cannot assign to immutable binding"),
        "{message}"
    );
}

#[test]
fn std_length_helpers() {
    let string_len = eval("std.string.len(\"hello\")");
    assert_eq!(expect_int(&string_len), 5);

    let array_len = eval("std.collections.len([1, 2, 3])");
    assert_eq!(expect_int(&array_len), 3);

    let map_len = eval(
        r#"
        std.collections.len({ "a": 1, "b": 2 })
        "#,
    );
    assert_eq!(expect_int(&map_len), 2);
}

#[test]
fn std_math_and_time() {
    let abs_val = eval("std.math.abs(-42)");
    assert_eq!(expect_int(&abs_val), 42);

    let sqrt_val = eval("std.math.sqrt(49)");
    match sqrt_val.0.as_ref() {
        ValueKind::Float(f) => assert!((*f - 7.0).abs() < 1e-6),
        _ => panic!("expected Float, found {}", sqrt_val.type_name()),
    }

    let now_val = eval("std.time.now()");
    match now_val.0.as_ref() {
        ValueKind::Float(f) => assert!(*f > 0.0),
        _ => panic!("expected Float, found {}", now_val.type_name()),
    }
}

#[test]
fn std_collections_helpers() {
    let pushed = eval("std.collections.push([1, 2], 3)");
    match pushed.0.as_ref() {
        ValueKind::Array(values) => {
            assert_eq!(values.len(), 3);
            assert_eq!(expect_int(&values[2]), 3);
        }
        _ => panic!("expected Array, found {}", pushed.type_name()),
    }

    let inserted = eval(
        r#"
        std.collections.insert({ "a": 1 }, "b", 2)
        "#,
    );
    let map = expect_map(&inserted);
    assert_eq!(map.len(), 2);
    assert_eq!(expect_int(map.get("b").unwrap()), 2);

    let keys = eval(
        r#"
        std.collections.keys({ "x": 1, "y": 2 })
        "#,
    );
    match keys.0.as_ref() {
        ValueKind::Array(values) => assert_eq!(values.len(), 2),
        _ => panic!("expected Array, found {}", keys.type_name()),
    }
}

#[test]
fn module_use_resolves_nested_paths() {
    let mut interpreter = Interpreter::new();

    let samedir_prefix = vec!["samedir".to_string()];
    interpreter
        .eval_source_with_prefix(
            r#"
            module samedir.one

            fn p() -> Int {
                return 7
            }
            "#,
            &samedir_prefix,
        )
        .expect("load samedir.one");

    let samedir_two_prefix = vec!["samedir".to_string(), "two".to_string()];
    interpreter
        .eval_source_with_prefix(
            r#"
            module two.impl

            fn p() -> Int {
                return 11
            }
            "#,
            &samedir_two_prefix,
        )
        .expect("load samedir.two.impl");

    interpreter
        .eval_source(
            r#"
            module main

            use samedir.one
            use samedir.two
            use samedir.two.impl as alias

            fn compute() -> Int {
                return one.p() + two.p() + alias.p()
            }
            "#,
        )
        .expect("load main module");

    let result = interpreter
        .eval_source(
            r#"
            use main
            main.compute()
            "#,
        )
        .expect("invoke main.compute()");
    assert_eq!(expect_int(&result), 29);
}

#[test]
fn module_access_requires_use() {
    let mut interpreter = Interpreter::new();
    interpreter
        .eval_source_with_prefix(
            r#"
            module helpers

            fn value() -> Int {
                return 13
            }
            "#,
            &[],
        )
        .expect("load helpers module");

    let err = interpreter
        .eval_source("helpers.value()")
        .expect_err("helpers should require use");
    let message = format!("{err}");
    assert!(
        message.contains("undefined variable `helpers`"),
        "{message}"
    );

    let value = interpreter
        .eval_source(
            r#"
            use helpers
            helpers.value()
            "#,
        )
        .expect("helpers.value() after use");
    assert_eq!(expect_int(&value), 13);
}

#[test]
fn std_fs_read_text() {
    let value = eval("std.fs.read_text(\"examples/quickstart.ns\")");
    match value.0.as_ref() {
        ValueKind::String(s) => assert!(s.contains("Hello from Narcissus")),
        _ => panic!("expected String, found {}", value.type_name()),
    }
}

#[test]
fn std_string_utilities() {
    let replaced = eval("std.string.replace(\"hello world\", \"world\", \"narcissus\")");
    match replaced.0.as_ref() {
        ValueKind::String(s) => assert_eq!(s, "hello narcissus"),
        _ => panic!("expected String, found {}", replaced.type_name()),
    }

    let starts = eval("std.string.starts_with(\"narcissus\", \"nar\")");
    assert!(expect_bool(&starts));

    let ends = eval("std.string.ends_with(\"narcissus\", \"sus\")");
    assert!(expect_bool(&ends));

    let joined = eval("std.string.join([\"a\", \"b\", \"c\"], \"-\")");
    match joined.0.as_ref() {
        ValueKind::String(s) => assert_eq!(s, "a-b-c"),
        _ => panic!("expected String, found {}", joined.type_name()),
    }
}

#[test]
fn std_collections_range_and_pop() {
    let range = eval("std.collections.range(0, 5)");
    match range.0.as_ref() {
        ValueKind::Array(values) => {
            assert_eq!(values.len(), 5);
            assert_eq!(expect_int(&values[0]), 0);
            assert_eq!(expect_int(&values[4]), 4);
        }
        _ => panic!("expected Array, found {}", range.type_name()),
    }

    let range_step = eval("std.collections.range_step(5, 0, -2)");
    match range_step.0.as_ref() {
        ValueKind::Array(values) => {
            assert_eq!(values.len(), 3);
            assert_eq!(expect_int(&values[0]), 5);
            assert_eq!(expect_int(&values[1]), 3);
            assert_eq!(expect_int(&values[2]), 1);
        }
        _ => panic!("expected Array, found {}", range_step.type_name()),
    }

    let popped = eval("std.collections.pop([1, 2, 3])");
    let map = expect_map(&popped);
    assert_eq!(expect_int(map.get("value").unwrap()), 3);
    match map.get("array").unwrap().0.as_ref() {
        ValueKind::Array(values) => assert_eq!(values.len(), 2),
        _ => panic!("expected Array in result array field"),
    }
}

#[test]
fn std_math_pow_round() {
    let rounded = eval("std.math.round(3.6)");
    match rounded.0.as_ref() {
        ValueKind::Float(f) => assert_eq!(*f, 4.0),
        _ => panic!("expected Float, found {}", rounded.type_name()),
    }

    let pow = eval("std.math.pow(2, 8)");
    match pow.0.as_ref() {
        ValueKind::Float(f) => assert_eq!(*f, 256.0),
        _ => panic!("expected Float, found {}", pow.type_name()),
    }
}

#[test]
fn std_fs_exists_and_remove() {
    let temp = tempdir().expect("create temp dir");
    let file_path = temp.path().join("sample.txt");
    let dir_path = temp.path().join("nested");

    let file_literal = path_literal(&file_path);
    let dir_literal = path_literal(&dir_path);

    let script = format!(
        r#"
        std.fs.create_dir_all("{dir}")
        std.fs.write_text("{file}", "hello")
        std.fs.exists("{dir}")
        "#,
        dir = dir_literal,
        file = file_literal
    );
    let exists_dir = eval(&script);
    assert!(expect_bool(&exists_dir));

    let exists_file = eval(&format!("std.fs.exists(\"{file}\")", file = file_literal));
    assert!(expect_bool(&exists_file));

    let removed = eval(&format!(
        r#"
        std.fs.remove("{file}")
        std.fs.exists("{file}")
        "#,
        file = file_literal
    ));
    assert!(!expect_bool(&removed));
}

#[test]
fn example_scripts_run() {
    for script in [
        "examples/quickstart.ns",
        "examples/collections.ns",
        "examples/patterns.ns",
    ] {
        let source = std::fs::read_to_string(script)
            .unwrap_or_else(|err| panic!("failed to read {script}: {err}"));
        let mut interpreter = Interpreter::new();
        assert!(
            interpreter.eval_source(&source).is_ok(),
            "{script} should run"
        );
    }
}
