use crate::TokenType::{
    self, And, Bang, BangEqual, Class, Comma, Dot, Else, Equal, EqualEqual, False, For, Fun,
    Greater, GreaterEqual, Identifier, If, LeftBrace, LeftParen, Less, LessEqual, Minus, Nil,
    Number, Or, Plus, Print, Return, RightBrace, RightParen, Semicolon, Slash, Star, Super, This,
    True, Var, While, EOF,
};
use crate::{Literal, Token};
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = [
        ("and", And),
        ("class", Class),
        ("else", Else),
        ("false", False),
        ("for", For),
        ("fun", Fun),
        ("if", If),
        ("nil", Nil),
        ("or", Or),
        ("print", Print),
        ("return", Return),
        ("super", Super),
        ("this", This),
        ("true", True),
        ("var", Var),
        ("while", While),
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
    pub(crate) fn new(source: String) -> Scanner {
        Scanner {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub(crate) fn scan_tokens(mut self) -> Vec<Token> {
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
        self.add_token(Number, Some(Literal::Number(value)))
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
        self.add_token(TokenType::String, Some(Literal::String(value)));
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

    fn add_token(&mut self, kind: TokenType, literal: Option<Literal>) {
        let text = self.source[self.start..self.current].to_string();
        self.tokens.push(Token {
            kind,
            lexeme: text,
            literal,
            line: self.line,
        });
    }
}
