use std::fmt::{Display, Formatter};
use std::fs;
use std::rc::Rc;

use crate::interpreter::{InterpretError, Interpreter};
use crate::parser::Parser;
use crate::scanner::Scanner;

mod interpreter;
mod parser;
mod scanner;

pub struct Lox {
    had_error: bool,
    had_runtime_error: bool,
    interpreter: Interpreter,
}

impl Lox {
    pub fn new() -> Lox {
        Lox {
            had_error: false,
            had_runtime_error: false,
            interpreter: Interpreter::default(),
        }
    }

    pub fn run_file(&mut self, path: String) {
        match fs::read_to_string(path) {
            Ok(source_code) => self.run(source_code),
            Err(e) => self.scan_error(0, e.to_string()),
        }
    }

    pub fn run(&mut self, source: impl Into<String>) {
        let tokens = Scanner::new(source.into(), self).scan_tokens();
        let statements = Parser::new(tokens, self).parse();

        // stop if there was a syntax error.
        if self.had_error() {
            return;
        }

        let statements = statements.expect("Expected statements cause had no error.");
        if let Err(e) = self.interpreter.interpret(statements) {
            self.runtime_error(e);
        }
    }

    pub fn had_error(&self) -> bool {
        self.had_error
    }

    pub fn had_runtime_error(&self) -> bool {
        self.had_runtime_error
    }

    pub fn set_had_error(&mut self, value: bool) {
        self.had_error = value;
    }

    pub fn scan_error(&mut self, line: usize, message: impl AsRef<str>) {
        self.report(line, "", message);
    }

    pub(crate) fn syntax_error(&mut self, token: &Token, message: impl AsRef<str>) {
        if let TokenKind::EOF = token.kind {
            self.report(token.line, " at end", message);
        } else {
            self.report(token.line, format!(" at '{}'", &token.lexeme), message)
        }
    }

    pub(crate) fn runtime_error(&mut self, error: InterpretError) {
        let (message, token) = match error {
            InterpretError::RuntimeError(e) => (e.message, e.token),
            InterpretError::Return { token, .. } => {
                ("Return statement outside function.".to_string(), token)
            }
        };
        eprintln!("{}\n[line {}]", message, token.line);
        self.had_runtime_error = true;
    }

    fn report(&mut self, line: usize, place: impl AsRef<str>, message: impl AsRef<str>) {
        eprintln!(
            "[line {}] Error{}: {}",
            line,
            place.as_ref(),
            message.as_ref()
        );
        self.had_error = true;
    }
}

struct FunctionStmt {
    name: Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
}

pub(crate) enum Stmt {
    Block(Vec<Stmt>),
    Expression(Expr),
    Function(Rc<FunctionStmt>),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Print(Expr),
    Return {
        keyword: Rc<Token>,
        value: Option<Expr>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
}

pub(crate) enum Expr {
    Assign {
        name: Rc<Token>,
        value: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Rc<Token>,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Rc<Token>,
        arguments: Vec<Expr>,
    },
    Grouping(Box<Expr>),
    Literal(Literal),
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Rc<Token>,
        right: Box<Expr>,
    },
    Variable {
        name: Rc<Token>,
    },
}

#[derive(Clone)]
pub(crate) enum Literal {
    Number(f64),
    String(Rc<String>),
    Boolean(bool),
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(value) => Display::fmt(value, f),
            Literal::String(value) => Display::fmt(value, f),
            Literal::Boolean(value) => Display::fmt(value, f),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

pub(crate) struct Token {
    kind: TokenKind,
    lexeme: Rc<String>,
    literal: Option<Literal>,
    line: usize,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) enum TokenKind {
    // single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // one or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // literals
    Identifier,
    String,
    Number,

    // keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EOF,
}
