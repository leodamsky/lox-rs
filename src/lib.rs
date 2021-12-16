use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::TokenKind::EOF;

mod ast_printer;
mod parser;
mod scanner;

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
    let parser = Parser::new(tokens);
    let expression = parser.parse();

    if let Some(expression) = expression {
        println!("{}", ast_printer::print_ast(&expression));
    } else {
        // stop if there was a syntax error.
        assert!(had_error());
        return Ok(());
    }

    Ok(())
}

fn scan_error(line: usize, message: impl AsRef<str>) {
    report(line, "", message);
}

fn syntax_error(token: &Token, message: impl AsRef<str>) {
    if token.kind == EOF {
        report(token.line, " at end", message);
    } else {
        report(token.line, format!(" at '{}'", &token.lexeme), message)
    }
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
    Literal(Literal),
    Unary {
        operator: Token,
        right: Box<Expr>,
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
