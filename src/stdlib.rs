use std::{fs, path::Path, thread, time::Duration};

use indexmap::IndexMap;

use crate::{
    diagnostics::{Diagnostic, DiagnosticKind, NarcissusError, Result},
    environment::EnvironmentRef,
    value::{NativeFunction, Value, ValueKind},
};

pub fn install(env: &EnvironmentRef) {
    let print = native("print", usize::MAX, io_print);
    let println = native("println", usize::MAX, io_println);
    let dbg = native("dbg", 1, io_dbg);
    let len_fn = native("len", 1, collections_len);
    let string_len_fn = len_fn.clone();

    {
        let mut scope = env.borrow_mut();
        scope.define("print".into(), print.clone(), false);
        scope.define("println".into(), println.clone(), false);
        scope.define("dbg".into(), dbg.clone(), false);
        scope.define("len".into(), len_fn.clone(), false);
    }

    let mut io = IndexMap::new();
    io.insert("print".into(), print);
    io.insert("println".into(), println);
    io.insert("dbg".into(), dbg.clone());
    io.insert("read_line".into(), native("read_line", 0, io_read_line));

    let mut string = IndexMap::new();
    string.insert("len".into(), string_len_fn.clone());
    string.insert("is_empty".into(), native("is_empty", 1, string_is_empty));
    string.insert("to_upper".into(), native("to_upper", 1, string_to_upper));
    string.insert("trim".into(), native("trim", 1, string_trim));
    string.insert("split".into(), native("split", 2, string_split));
    string.insert("replace".into(), native("replace", 3, string_replace));
    string.insert(
        "starts_with".into(),
        native("starts_with", 2, string_starts_with),
    );
    string.insert("ends_with".into(), native("ends_with", 2, string_ends_with));
    string.insert("join".into(), native("join", 2, string_join));

    let mut collections = IndexMap::new();
    collections.insert("len".into(), len_fn.clone());
    collections.insert("push".into(), native("push", 2, collections_push));
    collections.insert("insert".into(), native("insert", 3, collections_insert));
    collections.insert("keys".into(), native("keys", 1, collections_keys));
    collections.insert("values".into(), native("values", 1, collections_values));
    collections.insert("range".into(), native("range", 2, collections_range));
    collections.insert(
        "range_step".into(),
        native("range_step", 3, collections_range_step),
    );
    collections.insert("pop".into(), native("pop", 1, collections_pop));

    let mut math = IndexMap::new();
    math.insert("abs".into(), native("abs", 1, math_abs));
    math.insert("floor".into(), native("floor", 1, math_floor));
    math.insert("ceil".into(), native("ceil", 1, math_ceil));
    math.insert("sqrt".into(), native("sqrt", 1, math_sqrt));
    math.insert("round".into(), native("round", 1, math_round));
    math.insert("pow".into(), native("pow", 2, math_pow));

    let mut time = IndexMap::new();
    time.insert("now".into(), native("now", 0, time_now));
    time.insert("sleep".into(), native("sleep", 1, runtime_sleep));

    let mut runtime = IndexMap::new();
    runtime.insert("sleep".into(), native("sleep", 1, runtime_sleep));

    let mut fs_mod = IndexMap::new();
    fs_mod.insert("read_text".into(), native("read_text", 1, fs_read_text));
    fs_mod.insert("write_text".into(), native("write_text", 2, fs_write_text));
    fs_mod.insert("exists".into(), native("exists", 1, fs_exists));
    fs_mod.insert(
        "create_dir_all".into(),
        native("create_dir_all", 1, fs_create_dir_all),
    );
    fs_mod.insert("remove".into(), native("remove", 1, fs_remove));

    let mut net = IndexMap::new();
    net.insert(
        "unimplemented".into(),
        native("unimplemented", usize::MAX, net_unavailable),
    );

    let io_value = Value::module(vec!["std".into(), "io".into()], io);
    let string_value = Value::module(vec!["std".into(), "string".into()], string);
    let collections_value = Value::module(vec!["std".into(), "collections".into()], collections);
    let math_value = Value::module(vec!["std".into(), "math".into()], math);
    let time_value = Value::module(vec!["std".into(), "time".into()], time);
    let runtime_value = Value::module(vec!["std".into(), "runtime".into()], runtime);
    let fs_value = Value::module(vec!["std".into(), "fs".into()], fs_mod);
    let net_value = Value::module(vec!["std".into(), "net".into()], net);

    let mut std_exports = IndexMap::new();
    std_exports.insert("io".into(), io_value);
    std_exports.insert("string".into(), string_value);
    std_exports.insert("collections".into(), collections_value);
    std_exports.insert("math".into(), math_value);
    std_exports.insert("time".into(), time_value);
    std_exports.insert("runtime".into(), runtime_value);
    std_exports.insert("fs".into(), fs_value);
    std_exports.insert("net".into(), net_value);

    let std_value = Value::module(vec!["std".into()], std_exports);

    env.borrow_mut().define("std".into(), std_value, false);
}

fn native(name: &'static str, arity: usize, callback: fn(&[Value]) -> Result<Value>) -> Value {
    Value::new(ValueKind::NativeFunction(NativeFunction {
        name,
        arity,
        callback,
    }))
}

fn ensure_exact(args: &[Value], expected: usize, name: &str) -> Result<()> {
    if args.len() != expected && expected != usize::MAX {
        return Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            format!(
                "`{name}` expected {expected} arguments but received {}",
                args.len()
            ),
        )));
    }
    Ok(())
}

fn ensure_min(args: &[Value], min: usize, name: &str) -> Result<()> {
    if args.len() < min {
        return Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            format!(
                "`{name}` expected at least {min} arguments but received {}",
                args.len()
            ),
        )));
    }
    Ok(())
}

fn expect_string(value: &Value, name: &str) -> Result<String> {
    match &*value.0 {
        ValueKind::String(s) => Ok(s.clone()),
        _ => Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            format!("`{name}` expected String but found {}", value.type_name()),
        ))),
    }
}

fn expect_int(value: &Value, name: &str) -> Result<i64> {
    match &*value.0 {
        ValueKind::Int(n) => Ok(*n),
        _ => Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            format!("`{name}` expected Int but found {}", value.type_name()),
        ))),
    }
}

fn expect_number(value: &Value, name: &str) -> Result<f64> {
    match &*value.0 {
        ValueKind::Int(n) => Ok(*n as f64),
        ValueKind::Float(f) => Ok(*f),
        _ => Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            format!("`{name}` expected numeric but found {}", value.type_name()),
        ))),
    }
}

fn io_print(args: &[Value]) -> Result<Value> {
    ensure_min(args, 1, "std.io.print")?;
    for (idx, arg) in args.iter().enumerate() {
        if idx > 0 {
            print!(" ");
        }
        print!("{}", arg);
    }
    Ok(Value::unit())
}

fn io_println(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        println!();
        return Ok(Value::unit());
    }
    io_print(args)?;
    println!();
    Ok(Value::unit())
}

fn io_dbg(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.io.dbg")?;
    println!("{:?}", args[0]);
    Ok(args[0].clone())
}

fn io_read_line(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 0, "std.io.read_line")?;
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .map_err(|err| NarcissusError::from(err))?;
    if input.ends_with('\n') {
        input.pop();
        if input.ends_with('\r') {
            input.pop();
        }
    }
    Ok(Value::string(input))
}

fn string_is_empty(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.string.is_empty")?;
    let text = expect_string(&args[0], "std.string.is_empty")?;
    Ok(Value::bool(text.is_empty()))
}

fn string_to_upper(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.string.to_upper")?;
    let text = expect_string(&args[0], "std.string.to_upper")?;
    Ok(Value::string(text.to_uppercase()))
}

fn string_trim(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.string.trim")?;
    let text = expect_string(&args[0], "std.string.trim")?;
    Ok(Value::string(text.trim().to_string()))
}

fn string_split(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 2, "std.string.split")?;
    let text = expect_string(&args[0], "std.string.split")?;
    let separator = expect_string(&args[1], "std.string.split")?;
    if separator.is_empty() {
        return Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            "separator must not be empty",
        )));
    }
    let parts = text
        .split(&separator)
        .map(|part| Value::string(part.to_string()))
        .collect();
    Ok(Value::array(parts))
}

fn string_replace(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 3, "std.string.replace")?;
    let text = expect_string(&args[0], "std.string.replace")?;
    let from = expect_string(&args[1], "std.string.replace")?;
    let to = expect_string(&args[2], "std.string.replace")?;
    Ok(Value::string(text.replace(&from, &to)))
}

fn string_starts_with(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 2, "std.string.starts_with")?;
    let text = expect_string(&args[0], "std.string.starts_with")?;
    let prefix = expect_string(&args[1], "std.string.starts_with")?;
    Ok(Value::bool(text.starts_with(&prefix)))
}

fn string_ends_with(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 2, "std.string.ends_with")?;
    let text = expect_string(&args[0], "std.string.ends_with")?;
    let suffix = expect_string(&args[1], "std.string.ends_with")?;
    Ok(Value::bool(text.ends_with(&suffix)))
}

fn string_join(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 2, "std.string.join")?;
    let items = match &*args[0].0 {
        ValueKind::Array(values) => values,
        _ => {
            return Err(NarcissusError::from(Diagnostic::new(
                DiagnosticKind::Runtime,
                "`std.string.join` expects array of strings",
            )));
        }
    };
    let separator = expect_string(&args[1], "std.string.join")?;
    let mut output = String::new();
    for (idx, item) in items.iter().enumerate() {
        let piece = expect_string(item, "std.string.join")?;
        if idx > 0 {
            output.push_str(&separator);
        }
        output.push_str(&piece);
    }
    Ok(Value::string(output))
}

fn collections_len(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.collections.len")?;
    let value = &args[0];
    let len = match &*value.0 {
        ValueKind::String(s) => s.chars().count(),
        ValueKind::Array(arr) => arr.len(),
        ValueKind::Map(map) => map.len(),
        _ => {
            return Err(NarcissusError::from(Diagnostic::new(
                DiagnosticKind::Runtime,
                "len expects string, array, or map",
            )));
        }
    };
    Ok(Value::int(len as i64))
}

fn collections_push(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 2, "std.collections.push")?;
    match &*args[0].0 {
        ValueKind::Array(values) => {
            let mut new = values.clone();
            new.push(args[1].clone());
            Ok(Value::array(new))
        }
        _ => Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            "push expects array as first argument",
        ))),
    }
}

fn collections_insert(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 3, "std.collections.insert")?;
    match &*args[0].0 {
        ValueKind::Map(map) => {
            let key = expect_string(&args[1], "std.collections.insert")?;
            let mut new = map.clone();
            new.insert(key, args[2].clone());
            Ok(Value::map(new))
        }
        _ => Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            "insert expects map as first argument",
        ))),
    }
}

fn collections_keys(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.collections.keys")?;
    match &*args[0].0 {
        ValueKind::Map(map) => {
            let values = map.keys().cloned().map(Value::string).collect();
            Ok(Value::array(values))
        }
        _ => Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            "keys expects map",
        ))),
    }
}

fn collections_values(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.collections.values")?;
    match &*args[0].0 {
        ValueKind::Map(map) => Ok(Value::array(map.values().cloned().collect())),
        ValueKind::Array(arr) => Ok(Value::array(arr.clone())),
        _ => Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            "values expects map or array",
        ))),
    }
}

fn collections_range(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 2, "std.collections.range")?;
    collections_range_impl(args, None)
}

fn collections_range_step(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 3, "std.collections.range_step")?;
    collections_range_impl(args, Some(2))
}

fn collections_range_impl(args: &[Value], step_idx: Option<usize>) -> Result<Value> {
    let start = expect_int(&args[0], "std.collections.range")?;
    let end = expect_int(&args[1], "std.collections.range")?;
    let step = if let Some(idx) = step_idx {
        let raw = expect_int(&args[idx], "std.collections.range_step")?;
        if raw == 0 {
            return Err(NarcissusError::from(Diagnostic::new(
                DiagnosticKind::Runtime,
                "range step must be non-zero",
            )));
        }
        raw
    } else if start <= end {
        1
    } else {
        -1
    };

    let mut values = Vec::new();
    if step > 0 {
        let mut current = start;
        while current < end {
            values.push(Value::int(current));
            current += step;
        }
    } else {
        let mut current = start;
        while current > end {
            values.push(Value::int(current));
            current += step;
        }
    }
    Ok(Value::array(values))
}

fn collections_pop(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.collections.pop")?;
    match &*args[0].0 {
        ValueKind::Array(values) => {
            if values.is_empty() {
                return Err(NarcissusError::from(Diagnostic::new(
                    DiagnosticKind::Runtime,
                    "pop expects non-empty array",
                )));
            }
            let mut new = values.clone();
            let value = new.pop().unwrap();
            let mut result = IndexMap::new();
            result.insert("value".into(), value);
            result.insert("array".into(), Value::array(new));
            Ok(Value::map(result))
        }
        _ => Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            "pop expects array",
        ))),
    }
}

fn math_abs(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.math.abs")?;
    let number = expect_number(&args[0], "std.math.abs")?;
    if matches!(&*args[0].0, ValueKind::Int(_)) {
        Ok(Value::int(number.abs() as i64))
    } else {
        Ok(Value::float(number.abs()))
    }
}

fn math_floor(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.math.floor")?;
    let number = expect_number(&args[0], "std.math.floor")?;
    Ok(Value::float(number.floor()))
}

fn math_ceil(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.math.ceil")?;
    let number = expect_number(&args[0], "std.math.ceil")?;
    Ok(Value::float(number.ceil()))
}

fn math_sqrt(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.math.sqrt")?;
    let number = expect_number(&args[0], "std.math.sqrt")?;
    if number < 0.0 {
        return Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            "sqrt expects non-negative input",
        )));
    }
    Ok(Value::float(number.sqrt()))
}

fn math_round(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.math.round")?;
    let number = expect_number(&args[0], "std.math.round")?;
    Ok(Value::float(number.round()))
}

fn math_pow(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 2, "std.math.pow")?;
    let base = expect_number(&args[0], "std.math.pow")?;
    let exponent = expect_number(&args[1], "std.math.pow")?;
    Ok(Value::float(base.powf(exponent)))
}

fn time_now(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 0, "std.time.now")?;
    match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
        Ok(duration) => Ok(Value::float(duration.as_secs_f64())),
        Err(_) => Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            "system clock went backwards",
        ))),
    }
}

fn runtime_sleep(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.runtime.sleep")?;
    let millis = expect_number(&args[0], "std.runtime.sleep")?;
    if millis < 0.0 {
        return Err(NarcissusError::from(Diagnostic::new(
            DiagnosticKind::Runtime,
            "sleep duration must be non-negative",
        )));
    }
    thread::sleep(Duration::from_secs_f64(millis / 1000.0));
    Ok(Value::unit())
}

fn fs_read_text(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.fs.read_text")?;
    let path = expect_string(&args[0], "std.fs.read_text")?;
    match fs::read_to_string(&path) {
        Ok(contents) => Ok(Value::string(contents)),
        Err(err) => Err(io_error("std.fs.read_text", &path, err)),
    }
}

fn fs_write_text(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 2, "std.fs.write_text")?;
    let path = expect_string(&args[0], "std.fs.write_text")?;
    let contents = expect_string(&args[1], "std.fs.write_text")?;
    match fs::write(&path, contents) {
        Ok(_) => Ok(Value::unit()),
        Err(err) => Err(io_error("std.fs.write_text", &path, err)),
    }
}

fn fs_exists(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.fs.exists")?;
    let path = expect_string(&args[0], "std.fs.exists")?;
    Ok(Value::bool(Path::new(&path).exists()))
}

fn fs_create_dir_all(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.fs.create_dir_all")?;
    let path = expect_string(&args[0], "std.fs.create_dir_all")?;
    match fs::create_dir_all(&path) {
        Ok(_) => Ok(Value::unit()),
        Err(err) => Err(io_error("std.fs.create_dir_all", &path, err)),
    }
}

fn fs_remove(args: &[Value]) -> Result<Value> {
    ensure_exact(args, 1, "std.fs.remove")?;
    let path = expect_string(&args[0], "std.fs.remove")?;
    match fs::remove_file(&path) {
        Ok(_) => Ok(Value::unit()),
        Err(err) => Err(io_error("std.fs.remove", &path, err)),
    }
}

fn net_unavailable(_: &[Value]) -> Result<Value> {
    Err(NarcissusError::from(Diagnostic::new(
        DiagnosticKind::Runtime,
        "network module is not implemented in this runtime",
    )))
}

fn io_error(name: &str, path: &str, err: std::io::Error) -> NarcissusError {
    let mut diagnostic = Diagnostic::new(
        DiagnosticKind::Runtime,
        format!("`{name}` failed for `{path}`: {err}"),
    );
    if let Some(code) = err.raw_os_error() {
        diagnostic = diagnostic.with_note(format!("os error code: {code}"));
    }
    NarcissusError::from(diagnostic)
}
