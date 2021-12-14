use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use crate::scanner::Scanner;

mod ast_printer;
mod scanner;
mod reverse_polish;

static mut HAD_ERROR: bool = false;

pub fn had_error() -> bool {
    unsafe { HAD_ERROR }
}

pub fn set_had_error(value: bool) {
    unsafe { HAD_ERROR = value };
}

pub fn run(source: impl Into<String>) -> Result<(), Box<dyn Error>> {
    let scanner = Scanner::new(source.into());
    let tokens = scanner.scan_tokens();

    for token in tokens {
        println!("{:?}", token);
    }

    Ok(())
}

fn error(line: usize, message: String) {
    report(line, "", message);
}

fn report(line: usize, place: impl AsRef<str>, message: impl AsRef<str>) {
    eprintln!(
        "[line {}] Error{}: {}",
        line,
        place.as_ref(),
        message.as_ref()
    );
    unsafe {
        HAD_ERROR = true;
    }
}

#[derive(Debug)]
pub(crate) enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Literal(Option<Literal>),
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
}

#[derive(Debug)]
pub(crate) enum Literal {
    Number(f64),
    String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(value) => Display::fmt(value, f),
            Literal::String(value) => Display::fmt(value, f),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Token {
    kind: TokenType,
    lexeme: String,
    literal: Option<Literal>,
    line: usize,
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum TokenType {
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
