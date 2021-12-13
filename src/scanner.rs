use crate::scanner::TokenType::{
    Bang, BangEqual, Comma, Dot, Equal, EqualEqual, Greater, GreaterEqual, Identifier, LeftBrace,
    LeftParen, Less, LessEqual, Minus, Number, Plus, RightBrace, RightParen, Semicolon, Slash,
    Star, EOF,
};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::Debug;

trait Literal: Debug {}

impl Literal for f64 {}
impl Literal for String {}

#[derive(Debug)]
pub struct Token {
    kind: TokenType,
    lexeme: String,
    literal: Option<Box<dyn Literal>>,
    line: usize,
}

#[derive(Debug, Copy, Clone)]
pub enum TokenType {
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

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = [
        ("and", TokenType::And),
        ("class", TokenType::Class),
        ("else", TokenType::Else),
        ("false", TokenType::False),
        ("for", TokenType::For),
        ("fun", TokenType::Fun),
        ("if", TokenType::If),
        ("nil", TokenType::Nil),
        ("or", TokenType::Or),
        ("print", TokenType::Print),
        ("return", TokenType::Return),
        ("super", TokenType::Super),
        ("this", TokenType::This),
        ("true", TokenType::True),
        ("var", TokenType::Var),
        ("while", TokenType::While),
    ]
    .into_iter()
    .collect();
}

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        Scanner {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(mut self) -> Vec<Token> {
        while !self.is_at_end() {
            // we are at the beginning of the next lexeme
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token {
            kind: EOF,
            lexeme: String::new(),
            literal: None,
            line: self.line,
        });
        self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance().unwrap();
        match c {
            '(' => self.add_primitive_token(LeftParen),
            ')' => self.add_primitive_token(RightParen),
            '{' => self.add_primitive_token(LeftBrace),
            '}' => self.add_primitive_token(RightBrace),
            ',' => self.add_primitive_token(Comma),
            '.' => self.add_primitive_token(Dot),
            '-' => self.add_primitive_token(Minus),
            '+' => self.add_primitive_token(Plus),
            ';' => self.add_primitive_token(Semicolon),
            '*' => self.add_primitive_token(Star),
            '!' => {
                let token_type = if self.check_next('=') {
                    BangEqual
                } else {
                    Bang
                };
                self.add_primitive_token(token_type);
            }
            '=' => {
                let token_type = if self.check_next('=') {
                    EqualEqual
                } else {
                    Equal
                };
                self.add_primitive_token(token_type)
            }
            '<' => {
                let token_type = if self.check_next('=') {
                    LessEqual
                } else {
                    Less
                };
                self.add_primitive_token(token_type)
            }
            '>' => {
                let token_type = if self.check_next('=') {
                    GreaterEqual
                } else {
                    Greater
                };
                self.add_primitive_token(token_type)
            }
            '/' => {
                if self.check_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_primitive_token(Slash);
                }
            }
            ' ' | '\r' | '\t' => {
                // ignore whitespace 🐍
            }
            '\n' => self.line += 1,
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            _ => crate::error(self.line, "Unexpected character.".to_string()),
        }
    }

    fn identifier(&mut self) {
        while self.is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let token_type = KEYWORDS.get(text).copied().unwrap_or(Identifier);
        self.add_primitive_token(token_type);
    }

    fn number(&mut self) {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        // look for a fractional part
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            // consume the dot
            self.advance();

            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        let value = self.source[self.start..self.current]
            .parse::<f64>()
            .unwrap();
        self.add_token(Number, Some(Box::new(value)))
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            crate::error(self.line, "Unterminated string.".to_string());
            return;
        }

        // the closing ".
        self.advance();

        let value = self.source[(self.start + 1)..(self.current - 1)].to_string();
        self.add_token(TokenType::String, Some(Box::new(value)));
    }

    fn check_next(&mut self, expected: char) -> bool {
        let candidate = self.source.chars().nth(self.current);
        match candidate {
            None => false,
            Some(candidate) if candidate != expected => false,
            _ => {
                self.current += 1;
                true
            }
        }
    }

    fn peek(&self) -> char {
        self.source.chars().nth(self.current).unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        self.source.chars().nth(self.current + 1).unwrap_or('\0')
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        if self.is_digit(c) {
            return true;
        }
        match c {
            'a'..='z' | 'A'..='Z' | '_' => true,
            _ => false,
        }
    }

    fn is_digit(&self, c: char) -> bool {
        match c {
            '0'..='9' => true,
            _ => false,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.source.chars().nth(self.current);
        self.current += 1;
        c
    }

    fn add_primitive_token(&mut self, kind: TokenType) {
        self.add_token(kind, None);
    }

    fn add_token(&mut self, kind: TokenType, literal: Option<Box<dyn Literal>>) {
        let text = self.source[self.start..self.current].to_string();
        self.tokens.push(Token {
            kind,
            lexeme: text,
            literal,
            line: self.line,
        });
    }
}
