use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::{Expr, Literal, Stmt, Token, TokenKind};

#[derive(Debug)]
pub(crate) enum Value {
    Number(f64),
    String(Rc<String>),
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
    values: HashMap<Rc<String>, Rc<RefCell<Value>>>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub(crate) fn child(enclosing: Rc<RefCell<Environment>>) -> Environment {
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

trait Interpret<T> {
    // TODO: can it be refactor to &mut Environment ?
    fn interpret(&self, env: Rc<RefCell<Environment>>) -> Result<T, RuntimeError>;
}

const BREAK_INVALID_USE_MSG: &str = "'break' outside loop";

impl Interpret<()> for Stmt {
    fn interpret(&self, env: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
        fn execute_block(
            statements: &Vec<Stmt>,
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
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if is_truthy(&condition.interpret(Rc::clone(&env))?.borrow()) {
                    then_branch.interpret(env)?
                } else if let Some(else_branch) = else_branch {
                    else_branch.interpret(env)?
                }
            }
            Stmt::Print(expr) => {
                let value = expr.interpret(env)?;
                println!("{}", value.borrow());
            }
            Stmt::While { condition, body } => {
                while is_truthy(&condition.interpret(Rc::clone(&env))?.borrow()) {
                    if let Err(e) = body.interpret(Rc::clone(&env)) {
                        // FIXME: use a special kind of RuntimeError
                        //  instead of relying on the message itself
                        if &e.message == BREAK_INVALID_USE_MSG {
                            break;
                        } else {
                            return Err(e);
                        }
                    }
                }
            }
            Stmt::Var { name, initializer } => {
                let value = if let Some(initializer) = initializer {
                    initializer.interpret(Rc::clone(&env))?
                } else {
                    Value::Nil.into()
                };

                env.borrow_mut().define(&name.lexeme, value)
            }
            Stmt::Break(token) => return Err(RuntimeError {
                message: BREAK_INVALID_USE_MSG.to_string(),
                token: Rc::clone(token),
            }),
        }
        Ok(())
    }
}

impl Interpret<Rc<RefCell<Value>>> for Expr {
    fn interpret(&self, env: Rc<RefCell<Environment>>) -> Result<Rc<RefCell<Value>>, RuntimeError> {
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
                            })
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
                        })
                    }
                }
            }
            Expr::Grouping(expr) => expr.interpret(env)?,
            Expr::Literal(literal) => Value::from(literal).into(),
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left = left.interpret(Rc::clone(&env))?;
                if let TokenKind::Or = operator.kind {
                    if is_truthy(&left.borrow()) {
                        return Ok(left);
                    }
                } else {
                    if !is_truthy(&left.borrow()) {
                        return Ok(left);
                    }
                }
                right.interpret(env)?
            }
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

fn require_number_operand<T>(token: &Rc<Token>) -> Result<T, RuntimeError> {
    Err(RuntimeError {
        token: Rc::clone(token),
        message: "Operand must be a number.".to_string(),
    })
}

fn require_number_operands<T>(token: &Rc<Token>) -> Result<T, RuntimeError> {
    Err(RuntimeError {
        token: Rc::clone(token),
        message: "Operand must be a number.".to_string(),
    })
}
