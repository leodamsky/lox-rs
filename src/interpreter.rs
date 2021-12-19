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
    // so, Lox is single-threaded
    Ref(Rc<RefCell<Value>>),
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
            Value::Ref(value) => Display::fmt(&value.borrow(), f),
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

pub(crate) struct RuntimeError {
    pub(crate) message: String,
    pub(crate) token: Token,
}

#[derive(Default)]
pub(crate) struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub(crate) fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), RuntimeError> {
        for statement in statements {
            statement.interpret(&mut self.environment)?;
        }
        Ok(())
    }
}

#[derive(Default)]
struct Environment {
    values: HashMap<String, Rc<RefCell<Value>>>
}

impl Environment {
    fn define(&mut self, name: impl Into<String>, value: Value) {
        self.values.insert(name.into(), Rc::new(RefCell::new(value)));
        println!("{:?}", self.values);
    }

    fn get(&mut self, name: Token) -> Result<Value, RuntimeError> {
        match dbg!(self.values.get(dbg!(&name.lexeme))) {
            Some(value) => Ok(Value::Ref(Rc::clone(value))),
            None => Err(RuntimeError {
                message: format!("Undefined variable '{}'.", name.lexeme),
                token: name,
            })
        }
    }
}

trait Interpret {
    type Output;

    fn interpret(self, env: &mut Environment) -> Result<Self::Output, RuntimeError>;
}

impl Interpret for Stmt {
    type Output = ();

    fn interpret(self, env: &mut Environment) -> Result<Self::Output, RuntimeError> {
        match self {
            Stmt::Expression(expr) => {
                expr.interpret(env)?;
            },
            Stmt::Print(expr) => {
                let value = expr.interpret(env)?;
                println!("{}", value);
            }
            Stmt::Var { name, initializer } => {
                let value = if let Some(initializer) = initializer {
                    initializer.interpret(env)?
                } else {
                    Value::Nil
                };

                env.define(name.lexeme, value)
            }
        }
        Ok(())
    }
}

impl Interpret for Expr {
    type Output = Value;

    fn interpret(self, env: &mut Environment) -> Result<Self::Output, RuntimeError> {
        let expr = match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.interpret(env)?;
                let right = right.interpret(env)?;

                match operator.kind {
                    TokenKind::Minus => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => Value::Number(left - right),
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Slash => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => Value::Number(left / right),
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Star => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => Value::Number(left * right),
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Plus => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => Value::Number(left + right),
                        (Value::String(left), Value::String(right)) => Value::String(left + &right),
                        _ => {
                            return Err(RuntimeError {
                                token: operator,
                                message: "Operands must be two numbers or two string.".to_string(),
                            })
                        }
                    },
                    TokenKind::GreaterEqual => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Boolean(left >= right)
                        }
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Greater => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => Value::Boolean(left > right),
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::Less => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => Value::Boolean(left < right),
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::LessEqual => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => {
                            Value::Boolean(left <= right)
                        }
                        _ => return require_number_operands(operator),
                    },
                    TokenKind::BangEqual => Value::Boolean(!is_equal(left, right)),
                    TokenKind::EqualEqual => Value::Boolean(is_equal(left, right)),
                    _ => {
                        return Err(RuntimeError {
                            message: format!("Not supported binary operator: {}", operator.lexeme),
                            token: operator,
                        })
                    }
                }
            }
            Expr::Grouping(expr) => expr.interpret(env)?,
            Expr::Literal(literal) => literal.into(),
            Expr::Unary { operator, right } => {
                let right = right.interpret(env)?;

                match operator.kind {
                    TokenKind::Bang => Value::Boolean(!is_truthy(right)),
                    TokenKind::Minus => {
                        if let Value::Number(number) = right {
                            Value::Number(-number)
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
            Expr::Variable { name } => env.get(name)?,
        };
        Ok(expr)
    }
}

fn is_truthy(value: Value) -> bool {
    match value {
        Value::Boolean(b) => b,
        Value::Nil => false,
        _ => true,
    }
}

fn is_equal(left: Value, right: Value) -> bool {
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
