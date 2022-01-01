use crate::interpreter::{NativeFn, RuntimeError, Value};
use crate::{InterpretError, Token};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

pub(crate) struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<Rc<String>, Rc<RefCell<Value>>>,
}

impl Environment {
    fn global() -> Environment {
        let mut values = HashMap::new();

        values.insert(
            "clock".to_string().into(),
            Value::NativeFn(NativeFn {
                arity: 0,
                function: Box::new(|_, _| {
                    let time_millis = SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .expect("Time went backwards.")
                        .as_millis();
                    Ok(Value::Number(time_millis as f64).into())
                }),
            })
            .into(),
        );

        Environment {
            enclosing: None,
            values,
        }
    }

    pub(super) fn child(enclosing: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub(super) fn define(&mut self, name: Rc<String>, value: Rc<RefCell<Value>>) {
        self.values.insert(name, value);
    }

    fn ancestor(&self, distance: usize) -> Option<Rc<RefCell<Environment>>> {
        if distance == 0 {
            return None;
        }
        let mut environment = Rc::clone(self.enclosing.as_ref().unwrap());
        for _ in 0..(distance - 1) {
            let next = Rc::clone(environment.borrow().enclosing.as_ref().unwrap());
            environment = next;
        }
        Some(environment)
    }

    /// Returns pre-indexed (in Resolver) variable based on [distance].
    /// Panics if variable doesn't exist. This indicates that Resolver works incorrectly.
    pub(super) fn get_at(&self, distance: usize, name: &String) -> Rc<RefCell<Value>> {
        let value = if let Some(env) = self.ancestor(distance) {
            env.borrow().values.get(name).map(Rc::clone)
        } else {
            self.values.get(name).map(Rc::clone)
        };
        value.unwrap()
    }

    pub(super) fn get(&self, name: &Rc<Token>) -> Result<Rc<RefCell<Value>>, InterpretError> {
        if let Some(value) = self.values.get(&name.lexeme) {
            return Ok(Rc::clone(value));
        }
        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get(name);
        }
        Err(RuntimeError {
            message: format!("Undefined variable '{}'.", name.lexeme),
            token: Rc::clone(name),
        }
        .into())
    }

    pub(super) fn assign_at(
        &mut self,
        distance: usize,
        name: &Rc<Token>,
        value: Rc<RefCell<Value>>,
    ) -> Result<(), RuntimeError> {
        if let Some(env) = self.ancestor(distance) {
            env.borrow_mut().assign(name, value)
        } else {
            self.assign(name, value)
        }
    }

    pub(super) fn assign(&mut self, name: &Rc<Token>, value: Rc<RefCell<Value>>) -> Result<(), RuntimeError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(Rc::clone(&name.lexeme), value);
            return Ok(());
        }
        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow_mut().assign(name, value);
        }
        Err(RuntimeError {
            message: format!("Undefined variable '{}'.", name.lexeme),
            token: Rc::clone(name),
        })
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment::global()
    }
}
