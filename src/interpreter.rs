use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::{Expr, Literal, Stmt, Token, TokenKind};

#[derive(Debug)]
pub(crate) enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
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
        }
    }
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Number(n) => Value::Number(n),
            Literal::String(s) => Value::String(s),
            Literal::Boolean(b) => Value::Boolean(b),
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
    pub(crate) token: Token,
}

#[derive(Default)]
pub(crate) struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub(crate) fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), RuntimeError> {
        for statement in statements {
            statement.interpret(Rc::clone(&self.environment))?;
        }
        Ok(())
    }
}

#[derive(Default)]
struct Environment {
    values: HashMap<String, Option<Rc<RefCell<Value>>>>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub(crate) fn child(enclosing: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    fn define(&mut self, name: impl Into<String>, value: Option<Rc<RefCell<Value>>>) {
        self.values.insert(name.into(), value);
    }

    fn get(&self, name: Token) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        if let Some(value) = self.values.get(&name.lexeme) {
            return match value {
                Some(value) => Ok(Rc::clone(value)),
                None => Err(RuntimeError {
                    message: format!("Uninitialized variable '{}'.", name.lexeme),
                    token: name,
                }),
            };
        }
        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get(name);
        }
        Err(RuntimeError {
            message: format!("Undefined variable '{}'.", name.lexeme),
            token: name,
        })
    }

    fn assign(&mut self, name: Token, value: Rc<RefCell<Value>>) -> Result<(), RuntimeError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme, Some(value));
            return Ok(());
        }
        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow_mut().assign(name, value);
        }
        Err(RuntimeError {
            message: format!("Undefined variable '{}'.", &name.lexeme),
            token: name,
        })
    }
}

trait Interpret<T> {
    // TODO: can it be refactor to &mut Environment ?
    fn interpret(self, env: Rc<RefCell<Environment>>) -> Result<T, RuntimeError>;
}

impl Interpret<()> for Stmt {
    fn interpret(self, env: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
        fn execute_block(
            statements: Vec<Stmt>,
            env: Rc<RefCell<Environment>>,
        ) -> Result<(), RuntimeError> {
            for statement in statements {
                statement.interpret(Rc::clone(&env))?;
            }
            Ok(())
        }

        match self {
            Stmt::Block(statements) => {
                let enclosing = Rc::new(RefCell::new(Environment::child(env)));
                execute_block(statements, enclosing)?;
            }
            Stmt::Expression(expr) => {
                expr.interpret(env)?;
            }
            Stmt::Print(expr) => {
                let value = expr.interpret(env)?;
                println!("{}", RefCell::borrow(&value));
            }
            Stmt::Var { name, initializer } => {
                let value = if let Some(initializer) = initializer {
                    Some(initializer.interpret(Rc::clone(&env))?)
                } else {
                    None
                };

                env.borrow_mut().define(name.lexeme, value)
            }
        }
        Ok(())
    }
}

impl Interpret<Rc<RefCell<Value>>> for Expr {
    fn interpret(self, env: Rc<RefCell<Environment>>) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        let expr: Rc<RefCell<Value>> = match self {
            Expr::Assign { name, value } => {
                let value = value.interpret(Rc::clone(&env))?;
                env.borrow_mut().assign(name, Rc::clone(&value))?;
                value
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.interpret(Rc::clone(&env))?;
                let right = right.interpret(env)?;

                match operator.kind {
                    TokenKind::Minus => match (&*RefCell::borrow(&left), &*RefCell::borrow(&right))
                    {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Number(left - right).into()
                        }
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Slash => match (&*RefCell::borrow(&left), &*RefCell::borrow(&right))
                    {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Number(left / right).into()
                        }
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Star => {
                        match (&*RefCell::borrow(&left), &*RefCell::borrow(&right)) {
                            (Value::Number(left), Value::Number(right)) => {
                                Value::Number(left * right).into()
                            }
                            _ => return require_number_operands(operator),
                        }
                    }
                    TokenKind::Plus => {
                        match (&*RefCell::borrow(&left), &*RefCell::borrow(&right)) {
                            (Value::Number(left), Value::Number(right)) => {
                                Value::Number(left + right).into()
                            }
                            (Value::String(left), Value::String(right)) => {
                                Value::String(format!("{}{}", left, right)).into()
                            }
                            _ => {
                                return Err(RuntimeError {
                                    token: operator,
                                    message: "Operands must be two numbers or two string."
                                        .to_string(),
                                })
                            }
                        }
                    }
                    TokenKind::GreaterEqual => {
                        match (&*RefCell::borrow(&left), &*RefCell::borrow(&right)) {
                            (Value::Number(left), Value::Number(right)) => {
                                Value::Boolean(left >= right).into()
                            }
                            _ => return require_number_operands(operator),
                        }
                    }
                    TokenKind::Greater => {
                        match (&*RefCell::borrow(&left), &*RefCell::borrow(&right)) {
                            (Value::Number(left), Value::Number(right)) => {
                                Value::Boolean(left > right).into()
                            }
                            _ => return require_number_operands(operator),
                        }
                    }
                    TokenKind::Less => {
                        match (&*RefCell::borrow(&left), &*RefCell::borrow(&right)) {
                            (Value::Number(left), Value::Number(right)) => {
                                Value::Boolean(left < right).into()
                            }
                            _ => return require_number_operands(operator),
                        }
                    }
                    TokenKind::LessEqual => {
                        match (&*RefCell::borrow(&left), &*RefCell::borrow(&right)) {
                            (Value::Number(left), Value::Number(right)) => {
                                Value::Boolean(left <= right).into()
                            }
                            _ => return require_number_operands(operator),
                        }
                    }
                    TokenKind::BangEqual => Value::Boolean(!is_equal(
                        &*RefCell::borrow(&left),
                        &*RefCell::borrow(&right),
                    ))
                    .into(),
                    TokenKind::EqualEqual => Value::Boolean(is_equal(
                        &*RefCell::borrow(&left),
                        &*RefCell::borrow(&right),
                    ))
                    .into(),
                    _ => {
                        return Err(RuntimeError {
                            message: format!("Not supported binary operator: {}", operator.lexeme),
                            token: operator,
                        })
                    }
                }
            }
            Expr::Grouping(expr) => expr.interpret(env)?,
            Expr::Literal(literal) => Value::from(literal).into(),
            Expr::Unary { operator, right } => {
                let right = right.interpret(env)?;

                match operator.kind {
                    TokenKind::Bang => Value::Boolean(!is_truthy(&*RefCell::borrow(&right))).into(),
                    TokenKind::Minus => {
                        if let Value::Number(number) = &*RefCell::borrow(&right) {
                            Value::Number(-*number).into()
                        } else {
                            return require_number_operand(operator);
                        }
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!("Unsupported unary operator: {}", operator.lexeme),
                            token: operator,
                        })
                    }
                }
            }
            Expr::Variable { name } => env.borrow().get(name)?,
        };
        Ok(expr)
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

fn require_number_operand<T>(token: Token) -> Result<T, RuntimeError> {
    Err(RuntimeError {
        token,
        message: "Operand must be a number.".to_string(),
    })
}

fn require_number_operands<T>(token: Token) -> Result<T, RuntimeError> {
    Err(RuntimeError {
        token,
        message: "Operand must be a number.".to_string(),
    })
}
