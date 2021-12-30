use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::resolver::Binding;
use crate::{AssignExpr, Expr, FunctionStmt, Literal, Stmt, Token, TokenKind, VariableExpr};

pub(crate) enum Value {
    Number(f64),
    String(Rc<String>),
    Boolean(bool),
    Nil,
    Callable(Box<dyn Callable>),
    ClassInstance(ClassInstance),
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
            Value::Callable(call) => Display::fmt(call, f),
            Value::ClassInstance(instance) => Display::fmt(instance, f),
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

impl From<Value> for Rc<RefCell<Value>> {
    fn from(value: Value) -> Self {
        Rc::new(RefCell::new(value))
    }
}

pub(crate) enum InterpretError {
    Return {
        value: Rc<RefCell<Value>>,
        token: Rc<Token>,
    },
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

#[derive(Default)]
pub(crate) struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub(crate) fn interpret(
        &self,
        statements: Vec<Stmt>,
        binding: &Binding,
    ) -> Result<(), InterpretError> {
        for statement in statements {
            statement.interpret(
                Rc::clone(&self.environment),
                Rc::clone(&self.environment),
                &binding,
            )?;
        }
        Ok(())
    }
}

pub(crate) struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<Rc<String>, Rc<RefCell<Value>>>,
}

impl Environment {
    fn global() -> Environment {
        let mut values = HashMap::new();

        values.insert(
            "clock".to_string().into(),
            Value::Callable(Box::new(NativeFn {
                arity: 0,
                function: Box::new(|_, _| {
                    let time_millis = SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .expect("Time went backwards.")
                        .as_millis();
                    Ok(Value::Number(time_millis as f64))
                }),
            }))
            .into(),
        );

        Environment {
            enclosing: None,
            values,
        }
    }

    fn child(enclosing: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    fn define(&mut self, name: &Rc<String>, value: Rc<RefCell<Value>>) {
        self.values.insert(Rc::clone(name), value);
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

    fn get_at(
        &self,
        distance: usize,
        name: &Rc<Token>,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        if let Some(env) = self.ancestor(distance) {
            env.borrow().get(&name)
        } else {
            self.get(&name)
        }
    }

    fn get(&self, name: &Rc<Token>) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        if let Some(value) = self.values.get(&name.lexeme) {
            return Ok(Rc::clone(value));
        }
        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get(name);
        }
        Err(RuntimeError {
            message: format!("Undefined variable '{}'.", name.lexeme),
            token: Rc::clone(name),
        })
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

    fn assign(&mut self, name: &Rc<Token>, value: Rc<RefCell<Value>>) -> Result<(), RuntimeError> {
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

trait Interpret<T> {
    // TODO: can it be refactor to &mut Environment ?
    fn interpret(
        &self,
        global: Rc<RefCell<Environment>>,
        env: Rc<RefCell<Environment>>,
        binding: &Binding,
    ) -> Result<T, InterpretError>;
}

impl Interpret<()> for Stmt {
    fn interpret(
        &self,
        global: Rc<RefCell<Environment>>,
        env: Rc<RefCell<Environment>>,
        binding: &Binding,
    ) -> Result<(), InterpretError> {
        fn execute_block(
            statements: &Vec<Stmt>,
            global: Rc<RefCell<Environment>>,
            env: Rc<RefCell<Environment>>,
            binding: &Binding,
        ) -> Result<(), InterpretError> {
            for statement in statements {
                statement.interpret(Rc::clone(&global), Rc::clone(&env), binding)?;
            }
            Ok(())
        }

        match self {
            Stmt::Block(statements) => {
                let enclosing = Rc::new(RefCell::new(Environment::child(env)));
                execute_block(statements, global, enclosing, binding)?;
            }
            Stmt::Class { name, methods } => {
                env.borrow_mut().define(&name.lexeme, Value::Nil.into());

                let mut functions = HashMap::new();
                for method in methods {
                    let function = Function {
                        declaration: Rc::clone(method),
                        closure: Rc::clone(&env),
                    };
                    functions.insert(Rc::clone(&method.name.lexeme), Rc::new(function));
                }

                let class = Class {
                    name: Rc::clone(&name.lexeme),
                    methods: Rc::new(functions),
                };
                env.borrow_mut()
                    .assign(name, Value::Callable(Box::new(class)).into())?;
            }
            Stmt::Expression(expr) => {
                expr.interpret(global, env, binding)?;
            }
            Stmt::Function(stmt) => {
                let function = Function {
                    declaration: Rc::clone(stmt),
                    closure: Rc::clone(&env),
                };
                env.borrow_mut().define(
                    &stmt.name.lexeme,
                    Rc::new(RefCell::new(Value::Callable(Box::new(function)))),
                );
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if is_truthy(
                    &condition
                        .interpret(Rc::clone(&global), Rc::clone(&env), binding)?
                        .borrow(),
                ) {
                    then_branch.interpret(global, env, binding)?
                } else if let Some(else_branch) = else_branch {
                    else_branch.interpret(global, env, binding)?
                }
            }
            Stmt::Print(expr) => {
                let value = expr.interpret(global, env, binding)?;
                println!("{}", value.borrow());
            }
            Stmt::Return { keyword, value } => {
                let value = if let Some(expr) = value {
                    expr.interpret(global, env, binding)?
                } else {
                    Value::Nil.into()
                };

                return Err(InterpretError::Return {
                    value,
                    token: Rc::clone(keyword),
                });
            }
            Stmt::While { condition, body } => {
                while is_truthy(
                    &condition
                        .interpret(Rc::clone(&global), Rc::clone(&env), binding)?
                        .borrow(),
                ) {
                    body.interpret(Rc::clone(&global), Rc::clone(&env), binding)?;
                }
            }
            Stmt::Var { name, initializer } => {
                let value = if let Some(initializer) = initializer {
                    initializer.interpret(global, Rc::clone(&env), binding)?
                } else {
                    Value::Nil.into()
                };

                env.borrow_mut().define(&name.lexeme, value)
            }
        }
        Ok(())
    }
}

impl Interpret<Rc<RefCell<Value>>> for Expr {
    fn interpret(
        &self,
        global: Rc<RefCell<Environment>>,
        env: Rc<RefCell<Environment>>,
        binding: &Binding,
    ) -> Result<Rc<RefCell<Value>>, InterpretError> {
        fn look_up_variable(
            global: &Environment,
            env: &Environment,
            binding: &Binding,
            name: &Rc<Token>,
            id: usize,
        ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
            let distance = binding.resolve(id);
            if let Some(distance) = distance {
                env.get_at(distance, name)
            } else {
                global.get(name)
            }
        }
        let expr: Rc<RefCell<Value>> = match self {
            Expr::Assign(AssignExpr { id, name, value }) => {
                let value = value.interpret(Rc::clone(&env), Rc::clone(&env), binding)?;
                if let Some(distance) = binding.resolve(*id) {
                    env.borrow_mut()
                        .assign_at(distance, name, Rc::clone(&value))?;
                } else {
                    global.borrow_mut().assign(name, Rc::clone(&value))?;
                }
                value
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.interpret(Rc::clone(&global), Rc::clone(&env), binding)?;
                let right = right.interpret(global, env, binding)?;

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
                let callee = callee.interpret(Rc::clone(&global), Rc::clone(&env), binding)?;

                let mut args = vec![];
                for argument in arguments {
                    args.push(argument.interpret(Rc::clone(&global), Rc::clone(&env), binding)?);
                }

                // TODO: can I inline this?
                let result = match &*callee.borrow() {
                    Value::Callable(callable) => {
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
                        match callable.call(global, env, binding, args) {
                            Ok(value) => value.into(),
                            Err(InterpretError::Return { value, .. }) => value,
                            Err(e) => return Err(e),
                        }
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: "Can only call functions and classes.".to_string(),
                            token: Rc::clone(paren),
                        }
                        .into());
                    }
                };
                result
            }
            Expr::Get { object, name } => {
                let value = object.interpret(global, env, binding)?;
                // TODO: can I inline this?
                let result = match &*value.borrow() {
                    Value::ClassInstance(instance) => instance.get(name)?,
                    _ => {
                        return Err(RuntimeError {
                            token: Rc::clone(name),
                            message: "Only instances have properties.".to_string(),
                        }
                        .into())
                    }
                };
                result
            }
            Expr::Grouping(expr) => expr.interpret(global, env, binding)?,
            Expr::Literal(literal) => Value::from(literal).into(),
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left = left.interpret(Rc::clone(&global), Rc::clone(&env), binding)?;
                if let TokenKind::Or = operator.kind {
                    if is_truthy(&left.borrow()) {
                        return Ok(left);
                    }
                } else {
                    if !is_truthy(&left.borrow()) {
                        return Ok(left);
                    }
                }
                right.interpret(global, env, binding)?
            }
            Expr::Set {
                object,
                name,
                value,
            } => {
                let object = object.interpret(Rc::clone(&global), Rc::clone(&env), binding)?;
                let mut borrowed_object = object.borrow_mut();

                if let Value::ClassInstance(instance) = &mut *borrowed_object {
                    let value = value.interpret(global, env, binding)?;
                    instance.set(name, value)
                } else {
                    return Err(RuntimeError {
                        token: Rc::clone(name),
                        message: "Only instances have fields.".to_string(),
                    }
                    .into());
                }
            }
            Expr::Unary { operator, right } => {
                let right = right.interpret(global, env, binding)?;

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
            Expr::Variable(VariableExpr { id, name }) => {
                look_up_variable(&global.borrow(), &env.borrow(), binding, name, *id)?
            }
        };
        Ok(expr)
    }
}

pub(crate) trait Callable: Display {
    fn arity(&self) -> usize;

    fn call(
        &self,
        global: Rc<RefCell<Environment>>,
        env: Rc<RefCell<Environment>>,
        binding: &Binding,
        arguments: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Value, InterpretError>;
}

impl<T: Callable> Callable for Rc<T> {
    fn arity(&self) -> usize {
        <T as Callable>::arity(self)
    }

    fn call(
        &self,
        global: Rc<RefCell<Environment>>,
        env: Rc<RefCell<Environment>>,
        binding: &Binding,
        arguments: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Value, InterpretError> {
        <T as Callable>::call(self, global, env, binding, arguments)
    }
}

struct NativeFn {
    arity: usize,
    function: Box<
        dyn Fn(Rc<RefCell<Environment>>, Vec<Rc<RefCell<Value>>>) -> Result<Value, InterpretError>,
    >,
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
        _: Rc<RefCell<Environment>>,
        global: Rc<RefCell<Environment>>,
        _: &Binding,
        arguments: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Value, InterpretError> {
        let env = Rc::new(RefCell::new(Environment::child(global)));
        (self.function)(env, arguments)
    }
}

struct Function {
    declaration: Rc<FunctionStmt>,
    closure: Rc<RefCell<Environment>>,
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
        global: Rc<RefCell<Environment>>,
        _: Rc<RefCell<Environment>>,
        binding: &Binding,
        arguments: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Value, InterpretError> {
        let env = Rc::new(RefCell::new(Environment::child(Rc::clone(&self.closure))));
        for i in 0..self.declaration.params.len() {
            let name = &self.declaration.params[i].lexeme;
            let value = Rc::clone(&arguments[i]);
            env.borrow_mut().define(name, value);
        }
        for statement in self.declaration.body.iter() {
            statement.interpret(Rc::clone(&global), Rc::clone(&env), binding)?;
        }
        Ok(Value::Nil)
    }
}

struct Class {
    name: Rc<String>,
    methods: Rc<HashMap<Rc<String>, Rc<Function>>>,
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.name, f)
    }
}

impl Callable for Class {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _: Rc<RefCell<Environment>>,
        _: Rc<RefCell<Environment>>,
        _: &Binding,
        _: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Value, InterpretError> {
        let instance = ClassInstance::new(self);
        Ok(Value::ClassInstance(instance))
    }
}

pub(crate) struct ClassInstance {
    class_name: Rc<String>,
    fields: HashMap<Rc<String>, Rc<RefCell<Value>>>,
    methods: Rc<HashMap<Rc<String>, Rc<Function>>>,
}

impl ClassInstance {
    fn new(class: &Class) -> ClassInstance {
        ClassInstance {
            class_name: Rc::clone(&class.name),
            fields: HashMap::new(),
            methods: Rc::clone(&class.methods),
        }
    }

    fn get(&self, name: &Rc<Token>) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        if let Some(value) = self.fields.get(&name.lexeme) {
            return Ok(Rc::clone(value));
        }

        // TODO: delegate method lookup to class?
        if let Some(method) = self.methods.get(&name.lexeme) {
            return Ok(Value::Callable(Box::new(Rc::clone(method))).into());
        }

        Err(RuntimeError {
            message: format!("Undefined property '{}'.", name.lexeme),
            token: Rc::clone(name),
        })
    }

    fn set(&mut self, name: &Rc<Token>, value: Rc<RefCell<Value>>) -> Rc<RefCell<Value>> {
        self.fields
            .insert(Rc::clone(&name.lexeme), Rc::clone(&value));
        value
    }
}

impl Display for ClassInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class_name)
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
