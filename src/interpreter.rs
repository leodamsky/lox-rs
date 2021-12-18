use crate::{Expr, Literal, Token, TokenKind};
use std::fmt::{Display, Formatter};

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

pub(crate) struct RuntimeError {
    pub(crate) message: String,
    pub(crate) token: Token,
}

pub(crate) fn interpret(expr: Expr) -> Result<Value, RuntimeError> {
    let expr = match expr {
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            let left = interpret(*left)?;
            let right = interpret(*right)?;

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
                    (Value::Number(left), Value::Number(right)) => Value::Boolean(left >= right),
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
                    (Value::Number(left), Value::Number(right)) => Value::Boolean(left <= right),
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
        Expr::Grouping(expr) => interpret(*expr)?,
        Expr::Literal(literal) => literal.into(),
        Expr::Unary { operator, right } => {
            let right = interpret(*right)?;

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
    };
    Ok(expr)
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
