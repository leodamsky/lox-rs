use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::{Expr, FunctionStmt, Literal, Stmt, Token, TokenKind};

pub(crate) enum Value {
    Number(f64),
    String(Rc<String>),
    Boolean(bool),
    Nil,
    Callable(Box<dyn Callable>),
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

pub(crate) struct RuntimeError {
    pub(crate) message: String,
    pub(crate) token: Rc<Token>,
}

pub(crate) enum InterpretError {
    Return {
        value: Rc<RefCell<Value>>,
        token: Rc<Token>,
    },
    RuntimeError(RuntimeError),
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
    pub(crate) fn interpret(&self, statements: Vec<Stmt>) -> Result<(), InterpretError> {
        for statement in statements {
            statement.interpret(Rc::clone(&self.environment), Rc::clone(&self.environment))?;
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
    ) -> Result<T, InterpretError>;
}

impl Interpret<()> for Stmt {
    fn interpret(
        &self,
        global: Rc<RefCell<Environment>>,
        env: Rc<RefCell<Environment>>,
    ) -> Result<(), InterpretError> {
        fn execute_block(
            statements: &Vec<Stmt>,
            global: Rc<RefCell<Environment>>,
            env: Rc<RefCell<Environment>>,
        ) -> Result<(), InterpretError> {
            for statement in statements {
                statement.interpret(Rc::clone(&global), Rc::clone(&env))?;
            }
            Ok(())
        }

        match self {
            Stmt::Block(statements) => {
                let enclosing = Rc::new(RefCell::new(Environment::child(env)));
                execute_block(statements, global, enclosing)?;
            }
            Stmt::Expression(expr) => {
                expr.interpret(global, env)?;
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
                        .interpret(Rc::clone(&global), Rc::clone(&env))?
                        .borrow(),
                ) {
                    then_branch.interpret(global, env)?
                } else if let Some(else_branch) = else_branch {
                    else_branch.interpret(global, env)?
                }
            }
            Stmt::Print(expr) => {
                let value = expr.interpret(global, env)?;
                println!("{}", value.borrow());
            }
            Stmt::Return { keyword, value } => {
                let value = if let Some(expr) = value {
                    expr.interpret(global, env)?
                } else {
                    Value::Nil.into()
                };

                return Err(InterpretError::Return { value, token: Rc::clone(keyword) });
            }
            Stmt::While { condition, body } => {
                while is_truthy(
                    &condition
                        .interpret(Rc::clone(&global), Rc::clone(&env))?
                        .borrow(),
                ) {
                    body.interpret(Rc::clone(&global), Rc::clone(&env))?;
                }
            }
            Stmt::Var { name, initializer } => {
                let value = if let Some(initializer) = initializer {
                    initializer.interpret(global, Rc::clone(&env))?
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
    ) -> Result<Rc<RefCell<Value>>, InterpretError> {
        let expr: Rc<RefCell<Value>> = match self {
            Expr::Assign { name, value } => {
                let value = value.interpret(global, Rc::clone(&env))?;
                env.borrow_mut().assign(name, Rc::clone(&value))?;
                value
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.interpret(Rc::clone(&global), Rc::clone(&env))?;
                let right = right.interpret(global, env)?;

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
                            }.into())
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
                        }.into())
                    }
                }
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = callee.interpret(Rc::clone(&global), Rc::clone(&env))?;

                let mut args = vec![];
                for argument in arguments {
                    args.push(argument.interpret(Rc::clone(&global), Rc::clone(&env))?);
                }

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
                            }.into());
                        }
                        match callable.call(global, env, args) {
                            Ok(value) => value.into(),
                            Err(InterpretError::Return { value, .. }) => value,
                            Err(e) => return Err(e),
                        }
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: "Can only call functions and classes.".to_string(),
                            token: Rc::clone(paren),
                        }.into());
                    }
                };
                result
            }
            Expr::Grouping(expr) => expr.interpret(global, env)?,
            Expr::Literal(literal) => Value::from(literal).into(),
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left = left.interpret(Rc::clone(&global), Rc::clone(&env))?;
                if let TokenKind::Or = operator.kind {
                    if is_truthy(&left.borrow()) {
                        return Ok(left);
                    }
                } else {
                    if !is_truthy(&left.borrow()) {
                        return Ok(left);
                    }
                }
                right.interpret(global, env)?
            }
            Expr::Unary { operator, right } => {
                let right = right.interpret(global, env)?;

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
                        }.into())
                    }
                }
            }
            Expr::Variable { name } => env.borrow().get(name)?,
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
        arguments: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Value, InterpretError>;
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
        arguments: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Value, InterpretError> {
        let env = Rc::new(RefCell::new(Environment::child(Rc::clone(&self.closure))));
        for i in 0..self.declaration.params.len() {
            let name = &self.declaration.params[i].lexeme;
            let value = Rc::clone(&arguments[i]);
            env.borrow_mut().define(name, value);
        }
        for statement in self.declaration.body.iter() {
            statement.interpret(Rc::clone(&global), Rc::clone(&env))?;
        }
        Ok(Value::Nil)
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
    }.into())
}

fn require_number_operands<T>(token: &Rc<Token>) -> Result<T, InterpretError> {
    Err(RuntimeError {
        token: Rc::clone(token),
        message: "Operand must be a number.".to_string(),
    }.into())
}
