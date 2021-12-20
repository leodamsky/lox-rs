use crate::interpreter::{Interpreter, RuntimeError};
use std::error::Error;
use std::fmt::{Display, Formatter};

use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::TokenKind::EOF;

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

    pub fn run(&mut self, source: impl Into<String>) -> Result<(), Box<dyn Error>> {
        let tokens = Scanner::new(source.into(), self).scan_tokens();
        let statements = Parser::new(tokens, self).parse();

        // stop if there was a syntax error.
        if self.had_error() {
            return Ok(());
        }

        let statements = statements.expect("Expected statements cause had no error.");
        if let Err(e) = self.interpreter.interpret(statements) {
            self.runtime_error(e);
        }

        Ok(())
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

    pub(crate) fn scan_error(&mut self, line: usize, message: impl AsRef<str>) {
        self.report(line, "", message);
    }

    pub(crate) fn syntax_error(&mut self, token: &Token, message: impl AsRef<str>) {
        if token.kind == EOF {
            self.report(token.line, " at end", message);
        } else {
            self.report(token.line, format!(" at '{}'", &token.lexeme), message)
        }
    }

    pub(crate) fn runtime_error(&mut self, RuntimeError { message, token }: RuntimeError) {
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

#[derive(Debug)]
pub(crate) enum Stmt {
    Block(Vec<Stmt>),
    Expression(Expr),
    Print(Expr),
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
}

#[derive(Debug)]
pub(crate) enum Expr {
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Literal(Literal),
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
}

#[derive(Debug)]
pub(crate) enum Literal {
    Number(f64),
    String(String),
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

#[derive(Debug)]
pub(crate) struct Token {
    kind: TokenKind,
    lexeme: String,
    literal: Option<Literal>,
    line: usize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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
