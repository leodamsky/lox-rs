use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

mod env;

use crate::interpreter::env::{Environment, LocalEnvironment};
use crate::resolver::Binder;
use crate::{
    AssignExpr, Expr, FunctionStmt, Literal, Stmt, SuperExpr, ThisExpr, Token, TokenKind,
    VariableExpr,
};

type SharedValue = Rc<RefCell<Value>>;

/// Every runtime value must fit in either of these variants.
pub(crate) enum Value {
    Number(f64),
    String(Rc<String>),
    Boolean(bool),
    Nil,
    NativeFn(NativeFn),
    Function(Function),
    Class(Rc<RefCell<Class>>),
    ClassInstance(Rc<RefCell<ClassInstance>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => {
                let text = n.to_string();
                if text.ends_with(".0") {
                    write!(f, "{}", &text[..text.len() - 2])
                } else {
                    write!(f, "{}", text)
                }
            }
            Value::String(s) => Display::fmt(s, f),
            Value::Boolean(b) => Display::fmt(b, f),
            Value::Nil => write!(f, "nil"),
            Value::NativeFn(func) => Display::fmt(&func, f),
            Value::Function(func) => Display::fmt(&func, f),
            Value::Class(c) => Display::fmt(&c.borrow(), f),
            Value::ClassInstance(instance) => Display::fmt(&instance.borrow(), f),
        }
    }
}

impl From<&Literal> for Value {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(Rc::clone(s)),
            Literal::Boolean(b) => Value::Boolean(*b),
            Literal::Nil => Value::Nil,
        }
    }
}

impl From<Value> for SharedValue {
    fn from(value: Value) -> Self {
        Rc::new(RefCell::new(value))
    }
}

pub(crate) enum InterpretError {
    Return(SharedValue),
    RuntimeError(RuntimeError),
}

pub(crate) struct RuntimeError {
    pub(crate) message: String,
    pub(crate) token: Rc<Token>,
}

impl From<RuntimeError> for InterpretError {
    fn from(e: RuntimeError) -> Self {
        InterpretError::RuntimeError(e)
    }
}

pub(crate) struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub(crate) fn new(binder: Rc<RefCell<Binder>>) -> Interpreter {
        Interpreter {
            environment: Environment::new(binder),
        }
    }

    pub(crate) fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), InterpretError> {
        for statement in statements {
            statement.interpret(&mut self.environment)?;
        }
        Ok(())
    }
}

trait Interpret<T> {
    fn interpret(&self, env: &mut Environment) -> Result<T, InterpretError>;
}

impl Interpret<()> for Stmt {
    fn interpret(&self, env: &mut Environment) -> Result<(), InterpretError> {
        match self {
            Stmt::Block(statements) => {
                let handle = env.child();
                for statement in statements {
                    statement.interpret(env)?;
                }
                handle.restore_env(env);
            }
            Stmt::Class {
                name,
                superclass,
                methods,
            } => {
                let superclass = if let Some(superclass) = superclass {
                    let value = superclass.interpret(env)?;
                    let borrowed = value.borrow();
                    match &*borrowed {
                        Value::Class(class) => Some(Rc::clone(class)),
                        _ => {
                            return Err(RuntimeError {
                                message: "Superclass must be a class".to_string(),
                                token: Rc::clone(name),
                            }
                            .into());
                        }
                    }
                } else {
                    None
                };

                let index = env.define(Rc::clone(&name.lexeme), Value::Nil.into());

                let handle = if let Some(superclass) = &superclass {
                    let handle = env.child();
                    let value = Rc::new(RefCell::new(Value::Class(Rc::clone(superclass))));
                    env.define(Rc::new("super".to_string()), value);
                    Some(handle)
                } else {
                    None
                };

                let mut functions = HashMap::new();
                for method in methods {
                    let initializer = method.name.lexeme.as_str() == "init";
                    let function = Function {
                        declaration: Rc::clone(method),
                        closure: env.capture(),
                        initializer,
                    };
                    functions.insert(Rc::clone(&method.name.lexeme), Rc::new(function));
                }

                if let Some(handle) = handle {
                    handle.restore_env(env);
                }

                let class = Class::new(Rc::clone(&name.lexeme), superclass, functions);
                env.assign_last(index, name, Value::Class(class).into())?;
            }
            Stmt::Expression(expr) => {
                expr.interpret(env)?;
            }
            Stmt::Function(stmt) => {
                let function = Function {
                    declaration: Rc::clone(stmt),
                    closure: env.capture(),
                    initializer: false,
                };
                env.define(
                    Rc::clone(&stmt.name.lexeme),
                    Rc::new(RefCell::new(Value::Function(function))),
                );
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if is_truthy(&condition.interpret(env)?.borrow()) {
                    then_branch.interpret(env)?
                } else if let Some(else_branch) = else_branch {
                    else_branch.interpret(env)?
                }
            }
            Stmt::Print(expr) => {
                let value = expr.interpret(env)?;
                println!("{}", value.borrow());
            }
            Stmt::Return { value, .. } => {
                let value = if let Some(expr) = value {
                    expr.interpret(env)?
                } else {
                    Value::Nil.into()
                };

                return Err(InterpretError::Return(value));
            }
            Stmt::While { condition, body } => {
                while is_truthy(&condition.interpret(env)?.borrow()) {
                    body.interpret(env)?;
                }
            }
            Stmt::Var { name, initializer } => {
                let value = if let Some(initializer) = initializer {
                    initializer.interpret(env)?
                } else {
                    Value::Nil.into()
                };

                env.define(Rc::clone(&name.lexeme), value);
            }
        }
        Ok(())
    }
}

impl Interpret<SharedValue> for Expr {
    fn interpret(&self, env: &mut Environment) -> Result<SharedValue, InterpretError> {
        let expr: SharedValue = match self {
            Expr::Assign(AssignExpr { id, name, value }) => {
                let value = value.interpret(env)?;
                env.assign_by_id(*id, name, Rc::clone(&value))?;
                value
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.interpret(env)?;
                let right = right.interpret(env)?;

                match operator.kind {
                    TokenKind::Minus => match (&*left.borrow(), &*right.borrow()) {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Number(left - right).into()
                        }
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Slash => match (&*left.borrow(), &*right.borrow()) {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Number(left / right).into()
                        }
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Star => match (&*left.borrow(), &*right.borrow()) {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Number(left * right).into()
                        }
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Plus => match (&*left.borrow(), &*right.borrow()) {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Number(left + right).into()
                        }
                        (Value::String(left), Value::String(right)) => {
                            Value::String(Rc::new(format!("{}{}", left, right))).into()
                        }
                        _ => {
                            return Err(RuntimeError {
                                token: Rc::clone(operator),
                                message: "Operands must be two numbers or two string.".to_string(),
                            }
                            .into())
                        }
                    },
                    TokenKind::GreaterEqual => match (&*left.borrow(), &*right.borrow()) {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Boolean(left >= right).into()
                        }
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Greater => match (&*left.borrow(), &*right.borrow()) {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Boolean(left > right).into()
                        }
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Less => match (&*left.borrow(), &*right.borrow()) {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Boolean(left < right).into()
                        }
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::LessEqual => match (&*left.borrow(), &*right.borrow()) {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Boolean(left <= right).into()
                        }
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::BangEqual => {
                        Value::Boolean(!is_equal(&left.borrow(), &right.borrow())).into()
                    }
                    TokenKind::EqualEqual => {
                        Value::Boolean(is_equal(&left.borrow(), &right.borrow())).into()
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!("Not supported binary operator: {}", operator.lexeme),
                            token: Rc::clone(operator),
                        }
                        .into())
                    }
                }
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = callee.interpret(env)?;

                let mut args = vec![];
                for argument in arguments {
                    args.push(argument.interpret(env)?);
                }

                let call = |callable: &dyn Callable| -> Result<SharedValue, InterpretError> {
                    if args.len() != callable.arity() {
                        return Err(RuntimeError {
                            message: format!(
                                "Expected {} arguments but got {}.",
                                callable.arity(),
                                args.len()
                            ),
                            token: Rc::clone(paren),
                        }
                        .into());
                    }
                    match callable.call(env, args) {
                        Ok(value) => Ok(value),
                        Err(InterpretError::Return(value)) => Ok(value),
                        err => err,
                    }
                };

                return match &*callee.borrow() {
                    Value::NativeFn(func) => call(func),
                    Value::Function(func) => call(func),
                    Value::Class(class) => call(class.borrow().deref()),
                    _ => Err(RuntimeError {
                        message: "Can only call functions and classes.".to_string(),
                        token: Rc::clone(paren),
                    }
                    .into()),
                };
            }
            Expr::Get { object, name } => {
                let value = object.interpret(env)?;
                return match &*value.borrow() {
                    Value::ClassInstance(instance) => instance.borrow().get(name),
                    _ => Err(RuntimeError {
                        token: Rc::clone(name),
                        message: "Only instances have properties.".to_string(),
                    }
                    .into()),
                };
            }
            Expr::Grouping(expr) => expr.interpret(env)?,
            Expr::Literal(literal) => Value::from(literal).into(),
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left = left.interpret(env)?;
                if let TokenKind::Or = operator.kind {
                    if is_truthy(&left.borrow()) {
                        return Ok(left);
                    }
                } else if !is_truthy(&left.borrow()) {
                    return Ok(left);
                }

                right.interpret(env)?
            }
            Expr::Set {
                object,
                name,
                value,
            } => {
                let object = object.interpret(env)?;
                let borrowed_object: &Value = &object.borrow();

                if let Value::ClassInstance(instance) = borrowed_object {
                    let value = value.interpret(env)?;
                    instance.borrow_mut().set(name, value)
                } else {
                    return Err(RuntimeError {
                        token: Rc::clone(name),
                        message: "Only instances have fields.".to_string(),
                    }
                    .into());
                }
            }
            Expr::Super(SuperExpr {
                id,
                method: method_name,
                ..
            }) => {
                let superclass = env.look_up_keyword(*id, "super");
                let object = env.look_up_keyword_with_offset(*id, "this", 1);

                let method = match &*superclass.borrow() {
                    Value::Class(class) => class.borrow().find_method(&method_name.lexeme),
                    _ => unreachable!(),
                };

                let bound_method = if let Some(method) = method {
                    match &*object.borrow() {
                        Value::ClassInstance(instance) => method.bind(Rc::clone(instance)),
                        _ => unreachable!(),
                    }
                } else {
                    return Err(RuntimeError {
                        message: format!("Undefined property '{}'.", method_name.lexeme),
                        token: Rc::clone(method_name),
                    }
                    .into());
                };

                Rc::new(RefCell::new(Value::Function(bound_method)))
            }
            Expr::This(ThisExpr { id, keyword }) => env.look_up_variable(*id, keyword)?,
            Expr::Unary { operator, right } => {
                let right = right.interpret(env)?;

                match operator.kind {
                    TokenKind::Bang => Value::Boolean(!is_truthy(&right.borrow())).into(),
                    TokenKind::Minus => {
                        if let Value::Number(number) = &*right.borrow() {
                            Value::Number(-*number).into()
                        } else {
                            return require_number_operand(operator);
                        }
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!("Unsupported unary operator: {}", operator.lexeme),
                            token: Rc::clone(operator),
                        }
                        .into())
                    }
                }
            }
            Expr::Variable(VariableExpr { id, name }) => env.look_up_variable(*id, name)?,
        };
        Ok(expr)
    }
}

trait Callable: Display {
    fn arity(&self) -> usize;

    fn call(
        &self,
        env: &mut Environment,
        arguments: Vec<SharedValue>,
    ) -> Result<SharedValue, InterpretError>;
}

impl<T: Callable> Callable for Rc<T> {
    fn arity(&self) -> usize {
        <T as Callable>::arity(self)
    }

    fn call(
        &self,
        env: &mut Environment,
        arguments: Vec<SharedValue>,
    ) -> Result<SharedValue, InterpretError> {
        <T as Callable>::call(self, env, arguments)
    }
}

pub(crate) struct NativeFn {
    arity: usize,
    function:
        Box<dyn Fn(&mut Environment, Vec<SharedValue>) -> Result<SharedValue, InterpretError>>,
}

impl Display for NativeFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}

impl Callable for NativeFn {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(
        &self,
        env: &mut Environment,
        arguments: Vec<SharedValue>,
    ) -> Result<SharedValue, InterpretError> {
        let handle = env.native();
        let result = (self.function)(env, arguments);
        handle.restore_env(env);
        result
    }
}

pub(crate) struct Function {
    declaration: Rc<FunctionStmt>,
    closure: Option<Rc<RefCell<LocalEnvironment>>>,
    initializer: bool,
}

impl Function {
    fn bind(&self, instance: Rc<RefCell<ClassInstance>>) -> Function {
        let mut env = LocalEnvironment::new(self.closure.as_ref().map(Rc::clone));
        env.define(Value::ClassInstance(instance).into());
        Function {
            declaration: Rc::clone(&self.declaration),
            closure: Some(Rc::new(RefCell::new(env))),
            initializer: self.initializer,
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.declaration.name.lexeme)
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(
        &self,
        env: &mut Environment,
        arguments: Vec<SharedValue>,
    ) -> Result<SharedValue, InterpretError> {
        let handle = env.closure(self.closure.as_ref().map(Rc::clone));

        for (i, argument) in arguments.iter().enumerate() {
            let name = &self.declaration.params[i].lexeme;
            env.define(Rc::clone(name), Rc::clone(argument));
        }

        for statement in self.declaration.body.iter() {
            let result = statement.interpret(env);

            let error = match result {
                Ok(_) => continue,
                Err(e) => e,
            };
            // we've got an error, so we can discard current environment
            handle.restore_env(env);
            return if let InterpretError::Return(value) = error {
                // only method can be an initializer,
                // so we should always be able to get 'this'
                let value = if self.initializer {
                    self.closure.as_ref().unwrap().borrow().get_at(0, 0)
                } else {
                    value
                };
                Ok(value)
            } else {
                Err(error)
            };
        }

        handle.restore_env(env);

        if self.initializer {
            return Ok(self
                .closure
                .as_ref()
                .unwrap()
                .borrow()
                .get_at(0, 0));
        }

        // only 'return' statement can return values
        // and 'return' is propagated via InterpretError
        // to unwind the stack
        Ok(Value::Nil.into())
    }
}

pub(crate) struct Class {
    // TODO: I don't like this pattern, can we do better?
    this: Option<Rc<RefCell<Class>>>,
    name: Rc<String>,
    superclass: Option<Rc<RefCell<Class>>>,
    methods: HashMap<Rc<String>, Rc<Function>>,
}

impl Class {
    fn new(
        name: Rc<String>,
        superclass: Option<Rc<RefCell<Class>>>,
        methods: HashMap<Rc<String>, Rc<Function>>,
    ) -> Rc<RefCell<Class>> {
        let class = Class {
            this: None,
            name,
            superclass,
            methods,
        };
        let class = Rc::new(RefCell::new(class));
        class.borrow_mut().this = Some(Rc::clone(&class));
        class
    }

    fn this(&self) -> Rc<RefCell<Class>> {
        Rc::clone(self.this.as_ref().unwrap())
    }

    fn find_method(&self, name: &Rc<String>) -> Option<Rc<Function>> {
        if let Some(method) = self.methods.get(name) {
            return Some(Rc::clone(method));
        }

        if let Some(superclass) = &self.superclass {
            return superclass.borrow().find_method(name);
        }

        None
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.name, f)
    }
}

impl Callable for Class {
    fn arity(&self) -> usize {
        if let Some(initializer) = self.find_method(&Rc::new("init".to_string())) {
            initializer.arity()
        } else {
            0
        }
    }

    fn call(
        &self,
        env: &mut Environment,
        arguments: Vec<SharedValue>,
    ) -> Result<SharedValue, InterpretError> {
        let instance = ClassInstance::new(self.this());
        if let Some(initializer) = self.find_method(&Rc::new("init".to_string())) {
            initializer
                .bind(Rc::clone(&instance))
                .call(env, arguments)?;
        }
        Ok(Value::ClassInstance(instance).into())
    }
}

pub(crate) struct ClassInstance {
    this: Option<Rc<RefCell<ClassInstance>>>,
    class: Rc<RefCell<Class>>,
    fields: HashMap<Rc<String>, SharedValue>,
}

impl ClassInstance {
    fn new(class: Rc<RefCell<Class>>) -> Rc<RefCell<ClassInstance>> {
        let instance = ClassInstance {
            this: None,
            class,
            fields: HashMap::new(),
        };
        let instance = Rc::new(RefCell::new(instance));
        instance.borrow_mut().this = Some(Rc::clone(&instance));
        instance
    }

    fn this(&self) -> Rc<RefCell<ClassInstance>> {
        Rc::clone(self.this.as_ref().unwrap())
    }

    fn get(&self, name: &Rc<Token>) -> Result<SharedValue, InterpretError> {
        if let Some(value) = self.fields.get(&name.lexeme) {
            return Ok(Rc::clone(value));
        }

        if let Some(method) = self.class.borrow().find_method(&name.lexeme) {
            let method = method.bind(self.this());
            return Ok(Value::Function(method).into());
        }

        Err(RuntimeError {
            message: format!("Undefined property '{}'.", name.lexeme),
            token: Rc::clone(name),
        }
        .into())
    }

    fn set(&mut self, name: &Rc<Token>, value: SharedValue) -> SharedValue {
        self.fields
            .insert(Rc::clone(&name.lexeme), Rc::clone(&value));
        value
    }
}

impl Display for ClassInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class.borrow().name)
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Boolean(b) => *b,
        Value::Nil => false,
        _ => true,
    }
}

fn is_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Nil, Value::Nil) => true,
        (Value::Number(left), Value::Number(right)) => left == right,
        (Value::String(left), Value::String(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        _ => false,
    }
}

fn require_number_operand<T>(token: &Rc<Token>) -> Result<T, InterpretError> {
    Err(RuntimeError {
        token: Rc::clone(token),
        message: "Operand must be a number.".to_string(),
    }
    .into())
}

fn require_number_operands<T>(token: &Rc<Token>) -> Result<T, InterpretError> {
    Err(RuntimeError {
        token: Rc::clone(token),
        message: "Operand must be a number.".to_string(),
    }
    .into())
}
