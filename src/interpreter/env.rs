use crate::interpreter::{NativeFn, RuntimeError, SharedValue, Value};
use crate::{Binder, Token};
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
    binder: Rc<RefCell<Binder>>,
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
    pub(super) fn new(binder: Rc<RefCell<Binder>>) -> Environment {
        Environment {
            global: GlobalEnvironment::new(),
            local: None,
            binder,
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

    pub(super) fn define(&mut self, name: Rc<String>, value: SharedValue) -> Option<usize> {
        if let Some(local_env) = &self.local {
            Some(local_env.borrow_mut().define(value))
        } else {
            self.global.define(name, value);
            None
        }
    }

    pub(super) fn assign_by_id(
        &mut self,
        id: usize,
        name: &Rc<Token>,
        value: SharedValue,
    ) -> Result<(), RuntimeError> {
        if let Some(binding) = self.binder.borrow().resolve(id) {
            self.local
                .as_ref()
                .expect("local env to be present given binding")
                .borrow_mut()
                .assign_at(binding.hops, binding.index, value);
            Ok(())
        } else {
            // though, we have ID, this variable is stored in the global env,
            // so we don't need it
            self.global.assign(name, value)
        }
    }

    pub(super) fn assign_last(
        &mut self,
        index: Option<usize>,
        name: &Rc<Token>,
        value: SharedValue,
    ) -> Result<(), RuntimeError> {
        // if we have index, than we declared variable to the local environment
        if let Some(index) = index {
            self.local
                .as_ref()
                .unwrap()
                .borrow_mut()
                .assign_at(0, index, value);
            Ok(())
        } else {
            self.global.assign(name, value)
        }
    }

    pub(super) fn look_up_variable(
        &mut self,
        id: usize,
        name: &Rc<Token>,
    ) -> Result<SharedValue, RuntimeError> {
        if let Some(binding) = self.binder.borrow().resolve(id) {
            Ok(self
                .local
                .as_ref()
                .expect("local env to be present given binding")
                .borrow()
                .get_at(binding.hops, binding.index))
        } else {
            self.global.get(name)
        }
    }

    pub(super) fn look_up_keyword(&mut self, id: usize, name: &str) -> SharedValue {
        self.look_up_keyword_with_offset(id, name, 0)
    }

    pub(super) fn look_up_keyword_with_offset(
        &mut self,
        id: usize,
        name: &str,
        offset: usize,
    ) -> SharedValue {
        let binder = self.binder.borrow();
        let binding = binder
            .resolve(id)
            .unwrap_or_else(|| panic!("Interpreter hasn't defined '{}' keyword.", name));
        self.local
            .as_ref()
            .expect("local env to be present given binding")
            .borrow()
            .get_at(binding.hops - offset, binding.index)
    }
}

pub(crate) struct GlobalEnvironment {
    values: HashMap<Rc<String>, SharedValue>,
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

    pub(super) fn define(&mut self, name: Rc<String>, value: SharedValue) {
        self.values.insert(name, value);
    }

    pub(super) fn get(&self, name: &Rc<Token>) -> Result<SharedValue, RuntimeError> {
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
        value: SharedValue,
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
    values: Vec<SharedValue>,
}

impl LocalEnvironment {
    pub(super) fn new(enclosing: Option<Rc<RefCell<LocalEnvironment>>>) -> LocalEnvironment {
        LocalEnvironment {
            values: Vec::new(),
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

    pub(super) fn define(&mut self, value: SharedValue) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }

    /// Returns pre-indexed (in Resolver) variable based on [distance].
    /// Panics if variable doesn't exist. This indicates that Resolver works incorrectly.
    pub(super) fn get_at(&self, distance: usize, index: usize) -> Rc<RefCell<Value>> {
        if let Some(env) = self.ancestor(distance) {
            Rc::clone(&env.borrow().values[index])
        } else {
            Rc::clone(&self.values[index])
        }
    }

    fn assign_at(&mut self, distance: usize, index: usize, value: SharedValue) {
        if let Some(env) = self.ancestor(distance) {
            env.borrow_mut().values[index] = value;
        } else {
            self.values[index] = value;
        }
    }
}
