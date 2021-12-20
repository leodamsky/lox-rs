use std::iter::Peekable;
use std::vec::IntoIter;

use TokenKind::{
    Bang, BangEqual, Class, EqualEqual, False, For, Fun, Greater, GreaterEqual, If, LeftParen,
    Less, LessEqual, Minus, Nil, Number, Plus, Print, Return, Semicolon, Slash, Star, True, Var,
    While, EOF,
};

use crate::TokenKind::{Else, Equal, Identifier, LeftBrace, RightBrace, RightParen};
use crate::{Expr, Literal, Lox, Stmt, Token, TokenKind};

#[derive(Debug)]
pub(crate) struct ParseError {}

pub(crate) struct Parser<'a> {
    tokens: Peekable<IntoIter<Token>>,
    lox: &'a mut Lox,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: Vec<Token>, lox: &mut Lox) -> Parser {
        Parser {
            tokens: tokens.into_iter().peekable(),
            lox,
        }
    }

    pub(crate) fn parse(mut self) -> Result<Vec<Stmt>, Vec<ParseError>> {
        let mut statements = vec![];
        let mut errors = vec![];
        while self.peek().is_some() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => errors.push(e),
            }
        }
        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        let result = if self.try_consume(&Var).is_some() {
            self.var_declaration()
        } else {
            self.statement()
        };

        if result.is_err() {
            self.synchronize();
        }

        result
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(&Identifier, "Expect variable name.")?;

        let initializer = if let Some(_) = self.try_consume(&Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&Semicolon, "Expect ';' after variable declaration.")?;
        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.try_consume(&If).is_some() {
            self.if_statement()
        } else if self.try_consume(&Print).is_some() {
            self.print_statement()
        } else if self.try_consume(&LeftBrace).is_some() {
            Ok(Stmt::Block(self.block()?))
        } else {
            self.expression_statement()
        }
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(&RightParen, "Exprct ')' after if condition.")?;

        let then_branch = self.statement()?.into();
        let else_branch = if self.try_consume(&Else).is_some() {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(&Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(&Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(value))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = vec![];
        while let Some(token) = self.peek() {
            if token.kind == RightBrace {
                break;
            }
            statements.push(self.declaration()?);
        }
        self.consume(&RightBrace, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.equality()?;

        if let Some(equals) = self.try_consume(&Equal) {
            let value = self.assignment()?;

            if let Expr::Variable { name } = expr {
                return Ok(Expr::Assign {
                    name,
                    value: value.into(),
                });
            }

            self.error(&equals, "Invalid assignment target.");
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative(&[BangEqual, EqualEqual], Parser::comparison)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative(&[Greater, GreaterEqual, Less, LessEqual], Parser::term)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative(&[Minus, Plus], Parser::factor)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative(&[Slash, Star], Parser::unary)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(operator) = self.try_consume_any(&[Bang, Minus]) {
            let right = self.unary()?;
            Ok(Expr::Unary {
                operator,
                right: right.into(),
            })
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let candidate = self.advance().ok_or_else(|| {
            let token = self.tokens.peek().unwrap();
            self.lox
                .syntax_error(token, "Expect expression, got end of input.");
            ParseError {}
        })?;

        let expr = match candidate.kind {
            False => Expr::Literal(Literal::Boolean(false)),
            True => Expr::Literal(Literal::Boolean(true)),
            Nil => Expr::Literal(Literal::Nil),
            Number | TokenKind::String => {
                let literal = candidate
                    .literal
                    .expect("literal when it's number or string");
                Expr::Literal(literal)
            }
            LeftParen => {
                let expr = self.expression()?;
                self.consume(&RightParen, "Expect ')' after expression.")?;
                Expr::Grouping(expr.into())
            }
            Identifier => Expr::Variable { name: candidate },
            _ => return Err(self.error(&candidate, "Expect expression.")),
        };

        Ok(expr)
    }

    fn parse_left_associative<T>(
        &mut self,
        kinds: &[TokenKind],
        mut handle: T,
    ) -> Result<Expr, ParseError>
    where
        T: for<'r> FnMut(&'r mut Parser<'a>) -> Result<Expr, ParseError>,
    {
        let mut expr = handle(self)?;
        while let Some(operator) = self.try_consume_any(kinds) {
            let right = handle(self)?;
            expr = Expr::Binary {
                left: expr.into(),
                operator,
                right: right.into(),
            };
        }
        Ok(expr)
    }

    fn try_consume_any(&mut self, kinds: &[TokenKind]) -> Option<Token> {
        for kind in kinds {
            if let Some(token) = self.try_consume(kind) {
                return Some(token);
            }
        }
        None
    }

    fn try_consume(&mut self, kind: &TokenKind) -> Option<Token> {
        let token = self.peek()?;
        if &token.kind != kind {
            return None;
        }
        // never none
        self.tokens.next()
    }

    fn consume(&mut self, kind: &TokenKind, message: impl AsRef<str>) -> Result<Token, ParseError> {
        if let Some(token) = self.try_consume(kind) {
            Ok(token)
        } else {
            let token = self.tokens.peek().unwrap();
            self.lox.syntax_error(token, message);
            Err(ParseError {})
        }
    }

    fn advance(&mut self) -> Option<Token> {
        self.peek()?;
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        let token = self.force_peek();
        // do we really need EOF ???
        if token.kind == EOF {
            None
        } else {
            Some(token)
        }
    }

    fn force_peek(&mut self) -> &Token {
        self.tokens.peek().unwrap()
    }

    fn synchronize(&mut self) {
        while let Some(token) = self.advance() {
            match token.kind {
                Semicolon | Class | For | Fun | If | Print | Return | Var | While => return,
                _ => {}
            }
        }
    }

    fn error(&mut self, token: &Token, message: impl AsRef<str>) -> ParseError {
        self.lox.syntax_error(token, message);
        ParseError {}
    }
}
