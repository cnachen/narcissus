use std::{cell::RefCell, rc::Rc};

use indexmap::IndexMap;

use crate::{
    diagnostics::{Diagnostic, DiagnosticKind, NarcissusError, SourceSpan},
    value::Value,
};

pub type EnvironmentRef = Rc<RefCell<Environment>>;

#[derive(Debug, Default)]
pub struct Environment {
    parent: Option<EnvironmentRef>,
    bindings: IndexMap<String, Binding>,
}

impl Environment {
    pub fn new() -> EnvironmentRef {
        Rc::new(RefCell::new(Self {
            parent: None,
            bindings: IndexMap::new(),
        }))
    }

    pub fn with_parent(parent: EnvironmentRef) -> EnvironmentRef {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: IndexMap::new(),
        }))
    }

    pub fn define(&mut self, name: String, value: Value, mutable: bool) {
        self.define_with_visibility(name, value, mutable, true);
    }

    pub fn define_with_visibility(
        &mut self,
        name: String,
        value: Value,
        mutable: bool,
        exported: bool,
    ) {
        self.bindings.insert(
            name,
            Binding {
                value,
                mutable,
                exported,
            },
        );
    }

    pub fn define_alias(&mut self, name: String, value: Value) {
        self.define_with_visibility(name, value, false, false);
    }

    pub fn update_alias(&mut self, name: &str, value: Value) -> bool {
        if let Some(binding) = self.bindings.get_mut(name) {
            binding.value = value;
            true
        } else {
            false
        }
    }

    pub fn update_alias_ref(env: &EnvironmentRef, name: &str, value: Value) -> bool {
        env.borrow_mut().update_alias(name, value)
    }

    pub fn assign(
        env: &EnvironmentRef,
        name: &str,
        value: Value,
        span: SourceSpan,
    ) -> Result<(), NarcissusError> {
        if env.borrow().bindings.contains_key(name) {
            let mut env_mut = env.borrow_mut();
            let binding = env_mut.bindings.get_mut(name).unwrap();
            if !binding.mutable {
                return Err(NarcissusError::from(
                    Diagnostic::new(
                        DiagnosticKind::Runtime,
                        format!("cannot assign to immutable binding `{name}`"),
                    )
                    .with_span(span),
                ));
            }
            binding.value = value;
            return Ok(());
        }
        if let Some(parent) = env.borrow().parent.clone() {
            return Environment::assign(&parent, name, value, span);
        }
        Err(NarcissusError::from(
            Diagnostic::new(
                DiagnosticKind::Runtime,
                format!("undefined variable `{name}`"),
            )
            .with_span(span),
        ))
    }

    pub fn get(
        env: &EnvironmentRef,
        name: &str,
        span: SourceSpan,
    ) -> Result<Value, NarcissusError> {
        if let Some(binding) = env.borrow().bindings.get(name) {
            return Ok(binding.value.clone());
        }
        if let Some(parent) = env.borrow().parent.clone() {
            return Environment::get(&parent, name, span);
        }
        Err(NarcissusError::from(
            Diagnostic::new(
                DiagnosticKind::Runtime,
                format!("undefined variable `{name}`"),
            )
            .with_span(span),
        ))
    }

    pub fn snapshot(env: &EnvironmentRef) -> IndexMap<String, Value> {
        env.borrow()
            .bindings
            .iter()
            .filter_map(|(name, binding)| {
                if binding.exported {
                    Some((name.clone(), binding.value.clone()))
                } else {
                    None
                }
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub value: Value,
    pub mutable: bool,
    pub exported: bool,
}
