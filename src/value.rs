use std::{fmt, rc::Rc};

use indexmap::IndexMap;

use crate::{
    ast::Stmt,
    diagnostics::{Diagnostic, DiagnosticKind, NarcissusError, SourceSpan},
    environment::EnvironmentRef,
};

#[derive(Clone)]
pub struct Value(pub Rc<ValueKind>);

impl Value {
    pub fn new(kind: ValueKind) -> Self {
        Self(Rc::new(kind))
    }

    pub fn unit() -> Self {
        Self::new(ValueKind::Unit)
    }

    pub fn bool(value: bool) -> Self {
        Self::new(ValueKind::Bool(value))
    }

    pub fn int(value: i64) -> Self {
        Self::new(ValueKind::Int(value))
    }

    pub fn float(value: f64) -> Self {
        Self::new(ValueKind::Float(value))
    }

    pub fn string(value: impl Into<String>) -> Self {
        Self::new(ValueKind::String(value.into()))
    }

    pub fn array(values: Vec<Value>) -> Self {
        Self::new(ValueKind::Array(values))
    }

    pub fn map(entries: IndexMap<String, Value>) -> Self {
        Self::new(ValueKind::Map(entries))
    }

    pub fn module(name: Vec<String>, exports: IndexMap<String, Value>) -> Self {
        Self::new(ValueKind::Module(ModuleValue { name, exports }))
    }

    pub fn is_truthy(&self) -> bool {
        match &*self.0 {
            ValueKind::Unit => false,
            ValueKind::Bool(b) => *b,
            ValueKind::Int(n) => *n != 0,
            ValueKind::Float(f) => *f != 0.0,
            ValueKind::String(s) => !s.is_empty(),
            ValueKind::Array(values) => !values.is_empty(),
            ValueKind::Map(map) => !map.is_empty(),
            ValueKind::Module(module) => !module.exports.is_empty(),
            ValueKind::Function(_) | ValueKind::NativeFunction(_) => true,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match &*self.0 {
            ValueKind::Unit => "Unit",
            ValueKind::Bool(_) => "Bool",
            ValueKind::Int(_) => "Int",
            ValueKind::Float(_) => "Float",
            ValueKind::String(_) => "String",
            ValueKind::Array(_) => "Array",
            ValueKind::Map(_) => "Map",
            ValueKind::Module(_) => "Module",
            ValueKind::Function(_) => "Function",
            ValueKind::NativeFunction(_) => "Function",
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(&*self.0, ValueKind::Int(_))
    }

    pub fn expect_bool(&self, span: SourceSpan) -> Result<bool, NarcissusError> {
        match &*self.0 {
            ValueKind::Bool(b) => Ok(*b),
            _ => Err(NarcissusError::from(
                Diagnostic::new(
                    DiagnosticKind::Runtime,
                    format!("expected Bool, found {}", self.type_name()),
                )
                .with_span(span),
            )),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.0 {
            ValueKind::Unit => write!(f, "Unit"),
            ValueKind::Bool(b) => write!(f, "{b}"),
            ValueKind::Int(n) => write!(f, "{n}"),
            ValueKind::Float(n) => write!(f, "{n}"),
            ValueKind::String(s) => write!(f, "\"{s}\""),
            ValueKind::Array(values) => f.debug_list().entries(values.iter()).finish(),
            ValueKind::Map(map) => f.debug_map().entries(map.iter()).finish(),
            ValueKind::Module(module) => f
                .debug_struct("Module")
                .field("name", &module.name.join("."))
                .field("exports", &module.exports)
                .finish(),
            ValueKind::Function(fun) => write!(
                f,
                "<fn {}>",
                fun.name.clone().unwrap_or_else(|| "anonymous".into())
            ),
            ValueKind::NativeFunction(fun) => write!(f, "<native fn {}>", fun.name),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.0 {
            ValueKind::Unit => write!(f, "unit"),
            ValueKind::Bool(b) => write!(f, "{b}"),
            ValueKind::Int(n) => write!(f, "{n}"),
            ValueKind::Float(n) => write!(f, "{n}"),
            ValueKind::String(s) => write!(f, "{s}"),
            ValueKind::Array(values) => {
                write!(f, "[")?;
                for (idx, value) in values.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, "]")
            }
            ValueKind::Map(map) => {
                write!(f, "{{")?;
                for (idx, (key, value)) in map.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {value}")?;
                }
                write!(f, "}}")
            }
            ValueKind::Module(module) => write!(f, "<module {}>", module.name.join(".")),
            ValueKind::Function(fun) => write!(
                f,
                "<fn {}>",
                fun.name.clone().unwrap_or_else(|| "anonymous".into())
            ),
            ValueKind::NativeFunction(fun) => write!(f, "<native fn {}>", fun.name),
        }
    }
}

#[derive(Clone)]
pub enum ValueKind {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Array(Vec<Value>),
    Map(IndexMap<String, Value>),
    Module(ModuleValue),
    Function(UserFunction),
    NativeFunction(NativeFunction),
}

#[derive(Clone)]
pub struct ModuleValue {
    pub name: Vec<String>,
    pub exports: IndexMap<String, Value>,
}

#[derive(Clone)]
pub struct UserFunction {
    pub name: Option<String>,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
    pub env: EnvironmentRef,
    pub is_async: bool,
}

#[derive(Clone)]
pub struct NativeFunction {
    pub name: &'static str,
    pub arity: usize,
    pub callback: fn(&[Value]) -> Result<Value, NarcissusError>,
}

impl NativeFunction {
    pub fn call(&self, args: &[Value]) -> Result<Value, NarcissusError> {
        if self.arity != usize::MAX && args.len() != self.arity {
            return Err(NarcissusError::from(Diagnostic::new(
                DiagnosticKind::Runtime,
                format!(
                    "function `{}` expected {} arguments but received {}",
                    self.name,
                    self.arity,
                    args.len()
                ),
            )));
        }
        (self.callback)(args)
    }
}
