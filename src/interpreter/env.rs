use crate::interpreter::{NativeFn, RuntimeError, Value};
use crate::{Binding, Token};
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

pub(crate) struct Environment {
    global: GlobalEnvironment,
    // local env is used for child scopes,
    // so we need to share a reference to it
    local: Option<Rc<RefCell<LocalEnvironment>>>,
    binding: Rc<RefCell<Binding>>,
}

/// Closure that restores enclosing environment.
/// Must be called by the caller it is done with nested environment.
/// If the caller forgets to restore the environment,
/// [Handle] will panic at the time of de-allocation.
pub(super) struct Handle {
    executed: bool,
    enclosing_env: Option<Rc<RefCell<LocalEnvironment>>>,
}

impl Handle {
    // call by value to ensure we restore the environment only once
    pub(super) fn restore_env(mut self, env: &mut Environment) {
        self.executed = true;
        env.local = self.enclosing_env.as_ref().map(Rc::clone);
    }
}

impl Drop for Handle {
    fn drop(&mut self) {
        if !self.executed {
            panic!("Environment wasn't resorted after use of a child.")
        }
    }
}

impl Environment {
    pub(super) fn new(binding: Rc<RefCell<Binding>>) -> Environment {
        Environment {
            global: GlobalEnvironment::new(),
            local: None,
            binding,
        }
    }

    /// Re-shapes environment to be used by native functions,
    /// which should only rely on the global environment.
    pub(super) fn native(&mut self) -> Handle {
        self.closure(None)
    }

    /// Create a nested environment for a new scope.
    /// A new scope is created when:
    /// 1. We enter a block scope.
    /// 2. We enter a function (we need to bind arguments to parameters).
    /// 3. We enter a method (we need to bind 'super' and 'this').
    pub(super) fn child(&mut self) -> Handle {
        self.closure(self.capture())
    }

    /// Creates a nested environment but using external "captured" environment.
    /// Closures rely on variables that are declared around them,
    /// so to make sure we carry those variables,
    /// we need to capture a reference to the environment
    /// that encloses <b>closure declaration</b>.
    pub(super) fn closure(
        &mut self,
        mut enclosing: Option<Rc<RefCell<LocalEnvironment>>>,
    ) -> Handle {
        enclosing = Some(Rc::new(RefCell::new(LocalEnvironment::new(enclosing))));
        mem::swap(&mut enclosing, &mut self.local);
        Handle {
            executed: false,
            enclosing_env: enclosing,
        }
    }

    pub(super) fn capture(&self) -> Option<Rc<RefCell<LocalEnvironment>>> {
        self.local.as_ref().map(Rc::clone)
    }

    pub(super) fn define(&mut self, name: Rc<String>, value: Rc<RefCell<Value>>) {
        if let Some(local_env) = &self.local {
            local_env.borrow_mut().define(name, value);
        } else {
            self.global.define(name, value);
        }
    }

    pub(super) fn assign_by_id(
        &mut self,
        id: usize,
        name: &Rc<Token>,
        value: Rc<RefCell<Value>>,
    ) -> Result<(), RuntimeError> {
        if let Some(distance) = self.binding.borrow().resolve(id) {
            self.local
                .as_ref()
                .expect("local env to be present given binding")
                .borrow_mut()
                .assign_at(distance, name, value)
        } else {
            // though, we have ID, this variable is stored in the global env,
            // so we don't need it
            self.global.assign(name, value)
        }
    }

    pub(super) fn assign(
        &mut self,
        name: &Rc<Token>,
        value: Rc<RefCell<Value>>,
    ) -> Result<(), RuntimeError> {
        if let Some(local_env) = &self.local {
            if local_env
                .borrow_mut()
                .try_assign(name, Rc::clone(&value))
                .is_some()
            {
                return Ok(());
            }
        }
        self.global.assign(name, value)
    }

    pub(super) fn look_up_variable(
        &mut self,
        id: usize,
        name: &Rc<Token>,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        if let Some(distance) = self.binding.borrow().resolve(id) {
            Ok(self
                .local
                .as_ref()
                .expect("local env to be present given binding")
                .borrow()
                .get_at(distance, &name.lexeme))
        } else {
            self.global.get(name)
        }
    }

    pub(super) fn look_up_keyword(&mut self, id: usize, name: &str) -> Rc<RefCell<Value>> {
        self.look_up_keyword_with_offset(id, name, 0)
    }

    pub(super) fn look_up_keyword_with_offset(&mut self, id: usize, name: &str, offset: usize) -> Rc<RefCell<Value>> {
        let distance = self
            .binding
            .borrow()
            .resolve(id)
            .expect(&format!("Interpreter hasn't defined '{}' keyword.", name));
        self.local
            .as_ref()
            .expect("local env to be present given binding")
            .borrow()
            .get_at(distance - offset, &name.to_string())
    }
}

pub(crate) struct GlobalEnvironment {
    values: HashMap<Rc<String>, Rc<RefCell<Value>>>,
}

impl GlobalEnvironment {
    fn new() -> GlobalEnvironment {
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

        GlobalEnvironment { values }
    }

    pub(super) fn define(&mut self, name: Rc<String>, value: Rc<RefCell<Value>>) {
        self.values.insert(name, value);
    }

    pub(super) fn get(&self, name: &Rc<Token>) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        self.values
            .get(&name.lexeme)
            .map(Rc::clone)
            .ok_or_else(|| RuntimeError {
                message: format!("Undefined variable '{}'.", name.lexeme),
                token: Rc::clone(name),
            })
    }

    pub(super) fn assign(
        &mut self,
        name: &Rc<Token>,
        value: Rc<RefCell<Value>>,
    ) -> Result<(), RuntimeError> {
        if let Some(cur_value) = self.values.get_mut(&name.lexeme) {
            *cur_value = value;
            Ok(())
        } else {
            Err(RuntimeError {
                message: format!("Undefined variable '{}'.", name.lexeme),
                token: Rc::clone(name),
            })
        }
    }
}

pub(crate) struct LocalEnvironment {
    enclosing: Option<Rc<RefCell<LocalEnvironment>>>,
    values: HashMap<Rc<String>, Rc<RefCell<Value>>>,
}

impl LocalEnvironment {
    pub(super) fn new(enclosing: Option<Rc<RefCell<LocalEnvironment>>>) -> LocalEnvironment {
        LocalEnvironment {
            values: HashMap::new(),
            enclosing,
        }
    }

    fn ancestor(&self, distance: usize) -> Option<Rc<RefCell<LocalEnvironment>>> {
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

    pub(super) fn define(&mut self, name: Rc<String>, value: Rc<RefCell<Value>>) {
        self.values.insert(name, value);
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

    fn assign_at(
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

    fn assign(
        &mut self,
        name: &Rc<Token>,
        value: Rc<RefCell<Value>>,
    ) -> Result<(), RuntimeError> {
        if let Some(cur_value) = self.values.get_mut(&name.lexeme) {
            *cur_value = value;
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

    fn try_assign(&mut self, name: &Rc<Token>, value: Rc<RefCell<Value>>) -> Option<()> {
        if let Some(cur_value) = self.values.get_mut(&name.lexeme) {
            *cur_value = value;
            return Some(());
        }
        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow_mut().try_assign(name, value);
        }
        None
    }
}
