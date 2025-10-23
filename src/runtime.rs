use std::rc::Rc;

use indexmap::IndexMap;

use crate::{
    ast::{BinaryOp, Expr, ExprKind, Literal, Module, Pattern, Stmt, StmtKind, UnaryOp, WhenArm},
    diagnostics::{Diagnostic, DiagnosticKind, NarcissusError, Result, SourceSpan},
    environment::{Environment, EnvironmentRef},
    parser,
    value::{UserFunction, Value, ValueKind},
};

#[derive(Clone)]
struct PendingUse {
    env: EnvironmentRef,
    name: String,
    path: Vec<String>,
    span: SourceSpan,
}

#[derive(Default, Clone)]
pub struct ExecutionContext {
    pub module_prefix: Vec<String>,
    pub current_module: Vec<String>,
}

pub struct Interpreter {
    env: EnvironmentRef,
    context: ExecutionContext,
    modules: IndexMap<Vec<String>, Value>,
    pending_uses: Vec<PendingUse>,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Environment::new();
        let mut interpreter = Self {
            env,
            context: ExecutionContext::default(),
            modules: IndexMap::new(),
            pending_uses: Vec::new(),
        };
        interpreter.install_prelude();
        interpreter
    }

    pub fn with_context(context: ExecutionContext) -> Self {
        let env = Environment::new();
        let mut interpreter = Self {
            env,
            context,
            modules: IndexMap::new(),
            pending_uses: Vec::new(),
        };
        interpreter.install_prelude();
        interpreter
    }

    pub fn eval_source(&mut self, source: &str) -> Result<Value> {
        self.eval_source_with_prefix(source, &[])
    }

    pub fn eval_source_with_prefix(&mut self, source: &str, prefix: &[String]) -> Result<Value> {
        let prev_prefix = self.context.module_prefix.clone();
        self.context.module_prefix = prefix.to_vec();
        let module = parser::parse_module(source).map_err(NarcissusError::from)?;
        let result = self.eval_module(module);
        self.context.module_prefix = prev_prefix;
        result
    }

    pub fn eval_module(&mut self, module: Module) -> Result<Value> {
        let Module { name, items } = module;
        if let Some(declared) = name {
            let full_path = self.resolve_module_decl_path(&declared);
            let module_value = self.build_module_value(full_path.clone(), &items)?;
            self.register_module(full_path, module_value.clone());
            return Ok(module_value);
        }
        let prev_module = self.context.current_module.clone();
        self.context.current_module = self.context.module_prefix.clone();
        let mut last_value: Option<Value> = None;
        for stmt in items {
            match self.execute_statement(&stmt)? {
                FlowControl::Next => {}
                FlowControl::NextValue(value) => {
                    last_value = Some(value);
                }
                FlowControl::Return(value) => return Ok(value),
                FlowControl::Break(_) => {
                    return Err(NarcissusError::from(Diagnostic::new(
                        DiagnosticKind::Runtime,
                        "`break` outside loop",
                    )));
                }
                FlowControl::Continue => {
                    return Err(NarcissusError::from(Diagnostic::new(
                        DiagnosticKind::Runtime,
                        "`continue` outside loop",
                    )));
                }
            }
        }
        self.context.current_module = prev_module;
        self.resolve_pending_uses();
        self.ensure_imports_resolved()?;
        Ok(last_value.unwrap_or_else(Value::unit))
    }

    fn build_module_value(&mut self, path: Vec<String>, items: &[Stmt]) -> Result<Value> {
        let module_env = Environment::with_parent(Rc::clone(&self.env));
        let prev_env = Rc::clone(&self.env);
        let prev_module = self.context.current_module.clone();
        self.env = Rc::clone(&module_env);
        self.context.current_module = path.clone();

        for stmt in items {
            match self.execute_statement(stmt)? {
                FlowControl::Next | FlowControl::NextValue(_) => {}
                FlowControl::Return(_) => {
                    self.env = prev_env;
                    self.context.current_module = prev_module;
                    return Err(NarcissusError::from(
                        Diagnostic::new(
                            DiagnosticKind::Runtime,
                            "`return` is not allowed at module scope",
                        )
                        .with_span(stmt.span),
                    ));
                }
                FlowControl::Break(_) | FlowControl::Continue => {
                    self.env = prev_env;
                    self.context.current_module = prev_module;
                    return Err(NarcissusError::from(
                        Diagnostic::new(
                            DiagnosticKind::Runtime,
                            "loop control flow is not allowed at module scope",
                        )
                        .with_span(stmt.span),
                    ));
                }
            }
        }

        let exports = Environment::snapshot(&module_env);
        self.env = prev_env;
        self.context.current_module = prev_module;

        Ok(Value::module(path, exports))
    }

    fn resolve_module_decl_path(&self, name: &[String]) -> Vec<String> {
        self.combine_prefix(&self.context.module_prefix, name)
    }

    fn resolve_nested_module_path(&self, name: &[String]) -> Vec<String> {
        let base = if self.context.current_module.is_empty() {
            self.context.module_prefix.clone()
        } else {
            self.context.current_module.clone()
        };
        self.combine_prefix(&base, name)
    }

    fn combine_prefix(&self, prefix: &[String], name: &[String]) -> Vec<String> {
        if prefix.is_empty() {
            return name.to_vec();
        }
        if !name.is_empty() && name.len() >= prefix.len() && name[..prefix.len()] == prefix[..] {
            return name.to_vec();
        }
        if let (Some(last_prefix), Some(first_name)) = (prefix.last(), name.first()) {
            if last_prefix == first_name {
                let mut resolved = prefix.to_vec();
                resolved.extend_from_slice(&name[1..]);
                return resolved;
            }
        }
        let mut resolved = prefix.to_vec();
        resolved.extend_from_slice(name);
        resolved
    }

    fn register_module(&mut self, path: Vec<String>, module_value: Value) {
        self.modules.insert(path, module_value);
        self.resolve_pending_uses();
    }

    fn register_module_tree(&mut self, value: Value) {
        if let ValueKind::Module(module) = &*value.0 {
            if !module.name.is_empty() {
                self.modules.insert(module.name.clone(), value.clone());
            }
            for child in module.exports.values() {
                if matches!(&*child.0, ValueKind::Module(_)) {
                    self.register_module_tree(child.clone());
                }
            }
            self.resolve_pending_uses();
        }
    }

    fn resolve_pending_uses(&mut self) {
        let mut idx = 0;
        while idx < self.pending_uses.len() {
            let pending = &self.pending_uses[idx];
            if let Some(value) = self.resolve_symbol_path(&pending.path) {
                Environment::update_alias_ref(&pending.env, &pending.name, value);
                self.pending_uses.remove(idx);
            } else {
                idx += 1;
            }
        }
    }

    pub fn ensure_imports_resolved(&mut self) -> Result<()> {
        self.resolve_pending_uses();
        if let Some(pending) = self.pending_uses.first() {
            return Err(NarcissusError::from(
                Diagnostic::new(
                    DiagnosticKind::Runtime,
                    format!("unknown module `{}`", pending.path.join(".")),
                )
                .with_span(pending.span),
            ));
        }
        Ok(())
    }

    fn try_resolve_use_value(&self, path: &[String], span: SourceSpan) -> Result<Option<Value>> {
        if path.is_empty() {
            return Err(NarcissusError::from(
                Diagnostic::new(
                    DiagnosticKind::Runtime,
                    "use path must contain at least one segment",
                )
                .with_span(span),
            ));
        }

        if let Some(value) = self.resolve_symbol_path(path) {
            return Ok(Some(value));
        }

        if !self.context.current_module.is_empty() {
            let mut relative = self.context.current_module.clone();
            relative.extend_from_slice(path);
            if let Some(value) = self.resolve_symbol_path(&relative) {
                return Ok(Some(value));
            }
        }

        let matches: Vec<_> = self
            .modules
            .iter()
            .filter(|(module_path, _)| module_path.starts_with(path))
            .collect();
        if matches.len() == 1 {
            return Ok(Some(matches[0].1.clone()));
        } else if matches.len() > 1 {
            return Err(NarcissusError::from(
                Diagnostic::new(
                    DiagnosticKind::Runtime,
                    format!(
                        "use path `{}` is ambiguous; candidates: {}",
                        path.join("."),
                        matches
                            .iter()
                            .map(|(segments, _)| segments.join("."))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                )
                .with_span(span),
            ));
        }

        Ok(None)
    }

    fn resolve_symbol_path(&self, path: &[String]) -> Option<Value> {
        if let Some(value) = self.modules.get(path) {
            return Some(value.clone());
        }

        for split in (1..path.len()).rev() {
            let (module_part, remainder) = path.split_at(split);
            if let Some(module_value) = self.modules.get(module_part) {
                let mut current = module_value.clone();
                let mut ok = true;
                for segment in remainder {
                    if let Some(next) = Self::module_field(&current, segment) {
                        current = next;
                    } else {
                        ok = false;
                        break;
                    }
                }
                if ok {
                    return Some(current);
                }
            }
        }

        None
    }

    fn module_field(value: &Value, field: &str) -> Option<Value> {
        match &*value.0 {
            ValueKind::Module(module) => module.exports.get(field).cloned(),
            ValueKind::Map(map) => map.get(field).cloned(),
            _ => None,
        }
    }

    fn execute_statement(&mut self, stmt: &Stmt) -> Result<FlowControl> {
        match &stmt.kind {
            StmtKind::VarDecl {
                name,
                mutable,
                initializer,
                ..
            } => {
                let value = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::unit(),
                };
                self.env.borrow_mut().define(name.clone(), value, *mutable);
                Ok(FlowControl::Next)
            }
            StmtKind::ConstDecl { name, value, .. } => {
                let evaluated = self.evaluate(value)?;
                self.env.borrow_mut().define(name.clone(), evaluated, false);
                Ok(FlowControl::Next)
            }
            StmtKind::Function {
                name,
                params,
                body,
                is_async,
                ..
            } => {
                let param_names = params.iter().map(|p| p.name.clone()).collect();
                let function = UserFunction {
                    name: Some(name.clone()),
                    params: param_names,
                    body: body.clone(),
                    env: Rc::clone(&self.env),
                    is_async: *is_async,
                };
                self.env.borrow_mut().define(
                    name.clone(),
                    Value::new(ValueKind::Function(function)),
                    false,
                );
                Ok(FlowControl::Next)
            }
            StmtKind::Module { name, items } => {
                let full_path = self.resolve_nested_module_path(name);
                let module_value = self.build_module_value(full_path.clone(), items)?;
                if let Some(alias) = name.last().cloned().or_else(|| full_path.last().cloned()) {
                    self.env
                        .borrow_mut()
                        .define(alias, module_value.clone(), false);
                }
                self.register_module(full_path, module_value.clone());
                Ok(FlowControl::NextValue(module_value))
            }
            StmtKind::Use { path, alias } => {
                let binding = alias
                    .clone()
                    .or_else(|| path.last().cloned())
                    .ok_or_else(|| {
                        NarcissusError::from(
                            Diagnostic::new(
                                DiagnosticKind::Runtime,
                                "use path must contain at least one segment",
                            )
                            .with_span(stmt.span),
                        )
                    })?;
                let resolved = self.try_resolve_use_value(path, stmt.span)?;
                if let Some(value) = resolved {
                    self.env.borrow_mut().define_alias(binding, value);
                } else {
                    self.env
                        .borrow_mut()
                        .define_alias(binding.clone(), Value::unit());
                    self.pending_uses.push(PendingUse {
                        env: Rc::clone(&self.env),
                        name: binding,
                        path: path.clone(),
                        span: stmt.span,
                    });
                }
                Ok(FlowControl::Next)
            }
            StmtKind::Expr(expr) => {
                let value = self.evaluate(expr)?;
                Ok(FlowControl::NextValue(value))
            }
            StmtKind::Block(statements) => self.execute_block(statements),
            StmtKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if self.evaluate(condition)?.is_truthy() {
                    match self.execute_block(then_branch)? {
                        FlowControl::Next => Ok(FlowControl::Next),
                        FlowControl::NextValue(value) => Ok(FlowControl::NextValue(value)),
                        other => Ok(other),
                    }
                } else if let Some(branch) = else_branch {
                    match self.execute_block(branch)? {
                        FlowControl::Next => Ok(FlowControl::Next),
                        FlowControl::NextValue(value) => Ok(FlowControl::NextValue(value)),
                        other => Ok(other),
                    }
                } else {
                    Ok(FlowControl::Next)
                }
            }
            StmtKind::While { condition, body } => {
                loop {
                    if !self.evaluate(condition)?.is_truthy() {
                        break;
                    }
                    match self.execute_block(body)? {
                        FlowControl::Next => {}
                        FlowControl::NextValue(_) => {}
                        FlowControl::Continue => continue,
                        FlowControl::Break(None) => break,
                        FlowControl::Break(Some(value)) => {
                            return Ok(FlowControl::NextValue(value));
                        }
                        FlowControl::Return(value) => return Ok(FlowControl::Return(value)),
                    }
                }
                Ok(FlowControl::Next)
            }
            StmtKind::Loop { body } => {
                loop {
                    match self.execute_block(body)? {
                        FlowControl::Next => {}
                        FlowControl::NextValue(_) => {}
                        FlowControl::Continue => continue,
                        FlowControl::Break(None) => break,
                        FlowControl::Break(Some(value)) => {
                            return Ok(FlowControl::NextValue(value));
                        }
                        FlowControl::Return(value) => return Ok(FlowControl::Return(value)),
                    }
                }
                Ok(FlowControl::Next)
            }
            StmtKind::For {
                binding,
                iterable,
                body,
            } => {
                let iterable_value = self.evaluate(iterable)?;
                for item in self.iterate(iterable_value, iterable.span)? {
                    let child = Environment::with_parent(Rc::clone(&self.env));
                    let prev = Rc::clone(&self.env);
                    self.env = Rc::clone(&child);
                    self.env.borrow_mut().define(binding.clone(), item, true);
                    let flow = self.execute_block(body)?;
                    self.env = prev;
                    match flow {
                        FlowControl::Next => {}
                        FlowControl::NextValue(_) => {}
                        FlowControl::Continue => continue,
                        FlowControl::Break(None) => break,
                        FlowControl::Break(Some(value)) => {
                            return Ok(FlowControl::NextValue(value));
                        }
                        FlowControl::Return(value) => return Ok(FlowControl::Return(value)),
                    }
                }
                Ok(FlowControl::Next)
            }
            StmtKind::When { subject, arms } => {
                let value = self.evaluate(subject)?;
                for arm in arms {
                    if let Some(env) = self.match_arm(arm, &value)? {
                        let prev = Rc::clone(&self.env);
                        self.env = env;
                        let result = self.execute_block(&arm.body)?;
                        self.env = prev;
                        return Ok(result);
                    }
                }
                Ok(FlowControl::Next)
            }
            StmtKind::Return(expr) => {
                let value = match expr {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::unit(),
                };
                Ok(FlowControl::Return(value))
            }
            StmtKind::Break(expr) => {
                let value = match expr {
                    Some(expr) => Some(self.evaluate(expr)?),
                    None => None,
                };
                Ok(FlowControl::Break(value))
            }
            StmtKind::Continue => Ok(FlowControl::Continue),
        }
    }

    fn execute_block(&mut self, statements: &[Stmt]) -> Result<FlowControl> {
        let child = Environment::with_parent(Rc::clone(&self.env));
        let prev = Rc::clone(&self.env);
        self.env = Rc::clone(&child);
        let mut last_value: Option<Value> = None;
        for stmt in statements {
            let flow = self.execute_statement(stmt)?;
            match flow {
                FlowControl::Next => {}
                FlowControl::NextValue(value) => {
                    last_value = Some(value);
                }
                other => {
                    self.env = prev;
                    return Ok(other);
                }
            }
        }
        self.env = prev;
        if let Some(value) = last_value {
            Ok(FlowControl::NextValue(value))
        } else {
            Ok(FlowControl::Next)
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        match &expr.kind {
            ExprKind::Literal(lit) => Ok(self.literal(lit)),
            ExprKind::Variable(name) => Environment::get(&self.env, name, expr.span),
            ExprKind::Binary { op, left, right } => {
                let left_value = self.evaluate(left)?;
                let right_value = self.evaluate(right)?;
                self.binary(op, left_value, right_value, expr.span)
            }
            ExprKind::Unary { op, expr: right } => {
                let value = self.evaluate(right)?;
                self.unary(op, value, expr.span)
            }
            ExprKind::Assign { target, value } => {
                let value = self.evaluate(value)?;
                match &target.kind {
                    ExprKind::Variable(name) => {
                        Environment::assign(&self.env, name, value.clone(), target.span)?;
                        Ok(value)
                    }
                    ExprKind::Field {
                        target: owner,
                        field,
                    } => {
                        self.assign_field(owner, field, value.clone())?;
                        Ok(value)
                    }
                    ExprKind::Index {
                        target: owner,
                        index,
                    } => {
                        self.assign_index(owner, index, value.clone())?;
                        Ok(value)
                    }
                    _ => Err(NarcissusError::from(
                        Diagnostic::new(DiagnosticKind::Runtime, "invalid assignment target")
                            .with_span(target.span),
                    )),
                }
            }
            ExprKind::Call { callee, args } => {
                let callee_value = self.evaluate(callee)?;
                let mut eval_args = Vec::new();
                for arg in args {
                    eval_args.push(self.evaluate(arg)?);
                }
                self.call(callee_value, eval_args, expr.span)
            }
            ExprKind::ArrayLiteral(elements) => {
                let mut values = Vec::new();
                for element in elements {
                    values.push(self.evaluate(element)?);
                }
                Ok(Value::array(values))
            }
            ExprKind::MapLiteral(entries) => {
                let mut map = IndexMap::new();
                for (key_expr, value_expr) in entries {
                    let key_val = self.evaluate(key_expr)?;
                    let key = match &*key_val.0 {
                        ValueKind::String(s) => s.clone(),
                        ValueKind::Int(n) => n.to_string(),
                        _ => {
                            return Err(NarcissusError::from(
                                Diagnostic::new(
                                    DiagnosticKind::Runtime,
                                    "map keys must be String or Int",
                                )
                                .with_span(key_expr.span),
                            ));
                        }
                    };
                    let value = self.evaluate(value_expr)?;
                    map.insert(key, value);
                }
                Ok(Value::map(map))
            }
            ExprKind::TupleLiteral(elements) => {
                let mut values = Vec::new();
                for element in elements {
                    values.push(self.evaluate(element)?);
                }
                Ok(Value::array(values))
            }
            ExprKind::Group(inner) => self.evaluate(inner),
            ExprKind::Index { target, index } => {
                let target_value = self.evaluate(target)?;
                let index_value = self.evaluate(index)?;
                self.index(target_value, index_value, expr.span)
            }
            ExprKind::Field { target, field } => {
                let target_value = self.evaluate(target)?;
                self.field(target_value, field, expr.span)
            }
            ExprKind::Lambda { params, body } => {
                let param_names = params.iter().map(|p| p.name.clone()).collect();
                let function = UserFunction {
                    name: None,
                    params: param_names,
                    body: body.clone(),
                    env: Rc::clone(&self.env),
                    is_async: false,
                };
                Ok(Value::new(ValueKind::Function(function)))
            }
            ExprKind::Await(expr) | ExprKind::Try(expr) => self.evaluate(expr),
        }
    }

    fn literal(&self, literal: &Literal) -> Value {
        match literal {
            Literal::Int(n) => Value::int(*n),
            Literal::Float(n) => Value::float(*n),
            Literal::Bool(b) => Value::bool(*b),
            Literal::String(s) => Value::string(s.clone()),
            Literal::None => Value::unit(),
        }
    }

    fn binary(&self, op: &BinaryOp, left: Value, right: Value, span: SourceSpan) -> Result<Value> {
        use BinaryOp::*;
        match op {
            Add => self.numeric(left, right, span, |a, b| a + b),
            Sub => self.numeric(left, right, span, |a, b| a - b),
            Mul => self.numeric(left, right, span, |a, b| a * b),
            Div => self.numeric(left, right, span, |a, b| a / b),
            Mod => self.numeric(left, right, span, |a, b| a % b),
            Equal => Ok(Value::bool(self.equal(&left, &right))),
            NotEqual => Ok(Value::bool(!self.equal(&left, &right))),
            Less => self.comparison(left, right, span, |a, b| a < b),
            LessEqual => self.comparison(left, right, span, |a, b| a <= b),
            Greater => self.comparison(left, right, span, |a, b| a > b),
            GreaterEqual => self.comparison(left, right, span, |a, b| a >= b),
            And => Ok(Value::bool(left.is_truthy() && right.is_truthy())),
            Or => Ok(Value::bool(left.is_truthy() || right.is_truthy())),
        }
    }

    fn unary(&self, op: &UnaryOp, value: Value, span: SourceSpan) -> Result<Value> {
        match op {
            UnaryOp::Negate => match &*value.0 {
                ValueKind::Int(n) => Ok(Value::int(-n)),
                ValueKind::Float(n) => Ok(Value::float(-n)),
                _ => Err(NarcissusError::from(
                    Diagnostic::new(DiagnosticKind::Runtime, "unary `-` expects numeric value")
                        .with_span(span),
                )),
            },
            UnaryOp::Not => Ok(Value::bool(!value.is_truthy())),
        }
    }

    fn call(&mut self, callee: Value, args: Vec<Value>, span: SourceSpan) -> Result<Value> {
        match &*callee.0 {
            ValueKind::NativeFunction(fun) => fun.call(&args),
            ValueKind::Function(fun) => {
                if args.len() != fun.params.len() {
                    return Err(NarcissusError::from(
                        Diagnostic::new(
                            DiagnosticKind::Runtime,
                            format!(
                                "function expected {} arguments but received {}",
                                fun.params.len(),
                                args.len()
                            ),
                        )
                        .with_span(span),
                    ));
                }
                let new_env = Environment::with_parent(Rc::clone(&fun.env));
                for (name, value) in fun.params.iter().zip(args) {
                    new_env.borrow_mut().define(name.clone(), value, true);
                }
                let prev = Rc::clone(&self.env);
                self.env = new_env;
                let mut result = Value::unit();
                for stmt in &fun.body {
                    match self.execute_statement(stmt)? {
                        FlowControl::Next => continue,
                        FlowControl::NextValue(value) => {
                            result = value;
                        }
                        FlowControl::Return(value) => {
                            result = value;
                            break;
                        }
                        FlowControl::Break(_) | FlowControl::Continue => {
                            self.env = prev;
                            return Err(NarcissusError::from(Diagnostic::new(
                                DiagnosticKind::Runtime,
                                "loop control flow cannot escape closure",
                            )));
                        }
                    }
                }
                self.env = prev;
                Ok(result)
            }
            _ => Err(NarcissusError::from(
                Diagnostic::new(DiagnosticKind::Runtime, "value is not callable").with_span(span),
            )),
        }
    }

    fn assign_index(&mut self, target: &Expr, index: &Expr, value: Value) -> Result<()> {
        let target_value = self.evaluate(target)?;
        match &*target_value.0 {
            ValueKind::Array(elements) => {
                let idx_value = self.evaluate(index)?;
                let idx = match &*idx_value.0 {
                    ValueKind::Int(n) => *n as usize,
                    _ => {
                        return Err(NarcissusError::from(
                            Diagnostic::new(DiagnosticKind::Runtime, "array index must be Int")
                                .with_span(index.span),
                        ));
                    }
                };
                if idx >= elements.len() {
                    return Err(NarcissusError::from(
                        Diagnostic::new(
                            DiagnosticKind::Runtime,
                            format!("index {idx} out of bounds"),
                        )
                        .with_span(index.span),
                    ));
                }
                let mut new_elements = elements.clone();
                new_elements[idx] = value;
                self.write_back(target, Value::array(new_elements))
            }
            _ => Err(NarcissusError::from(
                Diagnostic::new(
                    DiagnosticKind::Runtime,
                    "index assignment expects array target",
                )
                .with_span(target.span),
            )),
        }
    }

    fn assign_field(&mut self, target: &Expr, field: &str, value: Value) -> Result<()> {
        let target_value = self.evaluate(target)?;
        match &*target_value.0 {
            ValueKind::Map(map) => {
                let mut new_map = map.clone();
                new_map.insert(field.to_string(), value);
                self.write_back(target, Value::map(new_map))
            }
            ValueKind::Module(_) => Err(NarcissusError::from(
                Diagnostic::new(DiagnosticKind::Runtime, "cannot assign to module fields")
                    .with_span(target.span),
            )),
            _ => Err(NarcissusError::from(
                Diagnostic::new(
                    DiagnosticKind::Runtime,
                    "field assignment expects map target",
                )
                .with_span(target.span),
            )),
        }
    }

    fn write_back(&mut self, target: &Expr, new_value: Value) -> Result<()> {
        match &target.kind {
            ExprKind::Variable(name) => {
                Environment::assign(&self.env, name, new_value, target.span)
            }
            ExprKind::Field {
                target: owner,
                field,
            } => {
                let owner_value = self.evaluate(owner)?;
                match &*owner_value.0 {
                    ValueKind::Map(map) => {
                        let mut new_map = map.clone();
                        new_map.insert(field.clone(), new_value);
                        self.write_back(owner, Value::map(new_map))
                    }
                    ValueKind::Module(_) => Err(NarcissusError::from(
                        Diagnostic::new(DiagnosticKind::Runtime, "cannot assign to module fields")
                            .with_span(target.span),
                    )),
                    _ => Err(NarcissusError::from(
                        Diagnostic::new(
                            DiagnosticKind::Runtime,
                            "field assignment expects map target",
                        )
                        .with_span(target.span),
                    )),
                }
            }
            ExprKind::Index {
                target: owner,
                index,
            } => {
                let owner_value = self.evaluate(owner)?;
                match &*owner_value.0 {
                    ValueKind::Array(elements) => {
                        let idx_value = self.evaluate(index)?;
                        let idx = match &*idx_value.0 {
                            ValueKind::Int(n) => *n as usize,
                            _ => {
                                return Err(NarcissusError::from(
                                    Diagnostic::new(
                                        DiagnosticKind::Runtime,
                                        "array index must be Int",
                                    )
                                    .with_span(index.span),
                                ));
                            }
                        };
                        if idx >= elements.len() {
                            return Err(NarcissusError::from(
                                Diagnostic::new(
                                    DiagnosticKind::Runtime,
                                    format!("index {idx} out of bounds"),
                                )
                                .with_span(index.span),
                            ));
                        }
                        let mut new_array = elements.clone();
                        new_array[idx] = new_value;
                        self.write_back(owner, Value::array(new_array))
                    }
                    _ => Err(NarcissusError::from(
                        Diagnostic::new(
                            DiagnosticKind::Runtime,
                            "index assignment expects array target",
                        )
                        .with_span(target.span),
                    )),
                }
            }
            _ => Err(NarcissusError::from(
                Diagnostic::new(
                    DiagnosticKind::Runtime,
                    "cannot assign to computed expression",
                )
                .with_span(target.span),
            )),
        }
    }

    fn numeric<F>(&self, left: Value, right: Value, span: SourceSpan, func: F) -> Result<Value>
    where
        F: Fn(f64, f64) -> f64,
    {
        let left_num = self.number(&left, span)?;
        let right_num = self.number(&right, span)?;
        let result = func(left_num, right_num);
        if left.is_int() && right.is_int() && result.fract() == 0.0 {
            Ok(Value::int(result as i64))
        } else {
            Ok(Value::float(result))
        }
    }

    fn comparison<F>(&self, left: Value, right: Value, span: SourceSpan, cmp: F) -> Result<Value>
    where
        F: Fn(f64, f64) -> bool,
    {
        let left_num = self.number(&left, span)?;
        let right_num = self.number(&right, span)?;
        Ok(Value::bool(cmp(left_num, right_num)))
    }

    fn number(&self, value: &Value, span: SourceSpan) -> Result<f64> {
        match &*value.0 {
            ValueKind::Int(n) => Ok(*n as f64),
            ValueKind::Float(n) => Ok(*n),
            _ => Err(NarcissusError::from(
                Diagnostic::new(
                    DiagnosticKind::Runtime,
                    format!("expected numeric value, found {}", value.type_name()),
                )
                .with_span(span),
            )),
        }
    }

    fn index(&self, target: Value, index: Value, span: SourceSpan) -> Result<Value> {
        match (&*target.0, &*index.0) {
            (ValueKind::Array(values), ValueKind::Int(idx)) => {
                let idx = *idx as usize;
                values.get(idx).cloned().ok_or_else(|| {
                    NarcissusError::from(
                        Diagnostic::new(
                            DiagnosticKind::Runtime,
                            format!("index {idx} out of bounds"),
                        )
                        .with_span(span),
                    )
                })
            }
            (ValueKind::String(text), ValueKind::Int(idx)) => {
                let idx = *idx as usize;
                text.chars()
                    .nth(idx)
                    .map(|ch| Value::string(ch.to_string()))
                    .ok_or_else(|| {
                        NarcissusError::from(
                            Diagnostic::new(
                                DiagnosticKind::Runtime,
                                format!("index {idx} out of bounds"),
                            )
                            .with_span(span),
                        )
                    })
            }
            _ => Err(NarcissusError::from(
                Diagnostic::new(
                    DiagnosticKind::Runtime,
                    "indexing expects array/string target with integer index",
                )
                .with_span(span),
            )),
        }
    }

    fn field(&self, target: Value, field: &str, span: SourceSpan) -> Result<Value> {
        match &*target.0 {
            ValueKind::Map(map) => map.get(field).cloned().ok_or_else(|| {
                NarcissusError::from(
                    Diagnostic::new(DiagnosticKind::Runtime, format!("missing field `{field}`"))
                        .with_span(span),
                )
            }),
            ValueKind::Module(module) => module.exports.get(field).cloned().ok_or_else(|| {
                NarcissusError::from(
                    Diagnostic::new(DiagnosticKind::Runtime, format!("missing field `{field}`"))
                        .with_span(span),
                )
            }),
            _ => Err(NarcissusError::from(
                Diagnostic::new(
                    DiagnosticKind::Runtime,
                    "field access expects map/module value",
                )
                .with_span(span),
            )),
        }
    }

    fn iterate(&self, value: Value, span: SourceSpan) -> Result<Vec<Value>> {
        match &*value.0 {
            ValueKind::Array(values) => Ok(values.clone()),
            ValueKind::String(text) => {
                Ok(text.chars().map(|c| Value::string(c.to_string())).collect())
            }
            ValueKind::Map(map) => Ok(map
                .iter()
                .map(|(key, value)| Value::array(vec![Value::string(key.clone()), value.clone()]))
                .collect()),
            ValueKind::Module(module) => Ok(module
                .exports
                .iter()
                .map(|(key, value)| Value::array(vec![Value::string(key.clone()), value.clone()]))
                .collect()),
            _ => Err(NarcissusError::from(
                Diagnostic::new(DiagnosticKind::Runtime, "value is not iterable").with_span(span),
            )),
        }
    }

    fn match_arm(&self, arm: &WhenArm, value: &Value) -> Result<Option<EnvironmentRef>> {
        let env = Environment::with_parent(Rc::clone(&self.env));
        if self.bind_pattern(&env, &arm.pattern, value)? {
            Ok(Some(env))
        } else {
            Ok(None)
        }
    }

    fn bind_pattern(&self, env: &EnvironmentRef, pattern: &Pattern, value: &Value) -> Result<bool> {
        match pattern {
            Pattern::Wildcard => Ok(true),
            Pattern::Literal(lit) => {
                let expected = self.literal(lit);
                Ok(self.equal(&expected, value))
            }
            Pattern::Identifier(name) => {
                env.borrow_mut().define(name.clone(), value.clone(), true);
                Ok(true)
            }
            Pattern::Tuple(patterns) => {
                if let ValueKind::Array(elements) = &*value.0 {
                    if patterns.len() != elements.len() {
                        return Ok(false);
                    }
                    for (pat, elem) in patterns.iter().zip(elements.iter()) {
                        if !self.bind_pattern(env, pat, elem)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        }
    }

    fn equal(&self, left: &Value, right: &Value) -> bool {
        match (&*left.0, &*right.0) {
            (ValueKind::Unit, ValueKind::Unit) => true,
            (ValueKind::Bool(a), ValueKind::Bool(b)) => a == b,
            (ValueKind::Int(a), ValueKind::Int(b)) => a == b,
            (ValueKind::Float(a), ValueKind::Float(b)) => (*a - *b).abs() < f64::EPSILON,
            (ValueKind::String(a), ValueKind::String(b)) => a == b,
            (ValueKind::Array(a), ValueKind::Array(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(l, r)| self.equal(l, r))
            }
            (ValueKind::Map(a), ValueKind::Map(b)) => {
                a.len() == b.len()
                    && a.iter().all(|(key, value)| {
                        b.get(key)
                            .map(|rhs| self.equal(value, rhs))
                            .unwrap_or(false)
                    })
            }
            _ => false,
        }
    }

    fn install_prelude(&mut self) {
        crate::stdlib::install(&self.env);
        if let Ok(std_value) = Environment::get(&self.env, "std", SourceSpan { start: 0, end: 0 }) {
            self.register_module_tree(std_value);
        }
    }
}

enum FlowControl {
    Next,
    NextValue(Value),
    Return(Value),
    Break(Option<Value>),
    Continue,
}
