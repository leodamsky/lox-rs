use std::iter::Peekable;
use std::rc::Rc;
use std::vec::IntoIter;

use TokenKind::{
    Bang, BangEqual, Class, EqualEqual, False, For, Fun, Greater, GreaterEqual, If, LeftParen,
    Less, LessEqual, Minus, Nil, Number, Plus, Print, Return, Semicolon, Slash, Star, True, Var,
    While, EOF,
};

use crate::TokenKind::{
    And, Comma, Dot, Else, Equal, Identifier, LeftBrace, Or, RightBrace, RightParen,
};
use crate::{AssignExpr, Expr, FunctionStmt, Literal, Lox, Stmt, Token, TokenKind, VariableExpr};

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
        let result = if self.try_consume(&Class).is_some() {
            self.class_declaration()
        } else if self.try_consume(&Fun).is_some() {
            self.function("function")
        } else if self.try_consume(&Var).is_some() {
            self.var_declaration()
        } else {
            self.statement()
        };

        if result.is_err() {
            self.synchronize();
        }

        result
    }

    fn class_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(&Identifier, "Expect class name.")?;
        self.consume(&LeftBrace, "Expect '{' before class body.")?;

        let mut methods = vec![];
        while !self.check(&RightBrace) && self.peek().is_some() {
            if let Stmt::Function(function) = self.function("method")? {
                methods.push(function);
            } else {
                unreachable!();
            }
        }

        self.consume(&RightBrace, "Expect '}' after class body.")?;
        Ok(Stmt::Class {
            name: Rc::new(name),
            methods,
        })
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, ParseError> {
        let name = self.consume(&Identifier, format!("Expect {} name.", kind))?;
        self.consume(&LeftParen, format!("Expect '(' after {} name.", kind))?;
        let mut parameters = vec![];
        if !self.check(&RightParen) {
            loop {
                parameters.push(self.consume(&Identifier, "Expect parameter name.")?);
                if parameters.len() > 255 {
                    let token = self.tokens.peek().unwrap();
                    self.lox
                        .syntax_error(token, "Can't have more than 255 parameters.");
                }

                if self.try_consume(&Comma).is_none() {
                    break;
                }
            }
        }
        self.consume(
            &RightParen,
            format!("Expect ')' after {} parameters.", kind),
        )?;

        self.consume(&LeftBrace, format!("Expect '{{' before {} body.", kind))?;
        let body = self.block()?;

        Ok(Stmt::Function(Rc::new(FunctionStmt {
            name,
            params: parameters,
            body,
        })))
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
        if self.try_consume(&For).is_some() {
            self.for_statement()
        } else if self.try_consume(&If).is_some() {
            self.if_statement()
        } else if self.try_consume(&Print).is_some() {
            self.print_statement()
        } else if let Some(keyword) = self.try_consume(&Return) {
            self.return_statement(keyword)
        } else if self.try_consume(&While).is_some() {
            self.while_statement()
        } else if self.try_consume(&LeftBrace).is_some() {
            Ok(Stmt::Block(self.block()?))
        } else {
            self.expression_statement()
        }
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&LeftParen, "Expect '(' after 'for'.")?;
        let initializer = if self.try_consume(&Semicolon).is_some() {
            None
        } else if self.try_consume(&Var).is_some() {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };
        let condition = if self.check(&Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(&Semicolon, "Expect ';' after loop condition.")?;
        let increment = if self.check(&RightParen) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(&RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(increment)]);
        }

        let condition = condition.unwrap_or_else(|| Expr::Literal(Literal::Boolean(true)));
        body = Stmt::While {
            condition,
            body: body.into(),
        };

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(&RightParen, "Expect ')' after if condition.")?;

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

    fn return_statement(&mut self, keyword: Token) -> Result<Stmt, ParseError> {
        let value = if !self.check(&Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Return {
            keyword: Rc::new(keyword),
            value,
        })
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(&Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(value))
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(&RightParen, "Expect ')' after condition.")?;

        let body = self.statement()?.into();

        Ok(Stmt::While { condition, body })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = vec![];
        while let Some(token) = self.peek() {
            if let TokenKind::RightBrace = token.kind {
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
        let expr = self.or()?;

        if let Some(equals) = self.try_consume(&Equal) {
            let value = self.assignment()?;

            if let Expr::Variable(VariableExpr { name, .. }) = expr {
                return Ok(Expr::Assign(AssignExpr::new(name, value.into())));
            } else if let Expr::Get { object, name } = expr {
                return Ok(Expr::Set {
                    object,
                    name,
                    value: Box::new(value),
                });
            }

            self.error(&equals, "Invalid assignment target.");
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.and()?;

        while let Some(operator) = self.try_consume(&Or) {
            let right = self.and()?;
            expr = Expr::Logical {
                left: expr.into(),
                operator,
                right: right.into(),
            };
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while let Some(operator) = self.try_consume(&And) {
            let right = self.equality()?;
            expr = Expr::Logical {
                left: expr.into(),
                operator,
                right: right.into(),
            };
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
                operator: operator.into(),
                right: right.into(),
            })
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if self.try_consume(&LeftParen).is_some() {
                expr = self.finish_call(expr)?;
            } else if self.try_consume(&Dot).is_some() {
                let name = self.consume(&Identifier, "Expect property name after '.'.")?;
                expr = Expr::Get {
                    object: Box::new(expr),
                    name: Rc::new(name),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, expr: Expr) -> Result<Expr, ParseError> {
        let mut arguments = vec![];

        if !self.check(&RightParen) {
            loop {
                arguments.push(self.expression()?);
                if self.try_consume(&Comma).is_none() {
                    break;
                }
            }
        }

        if arguments.len() > 255 {
            let token = self.tokens.peek().unwrap();
            self.lox
                .syntax_error(token, "Can't have more than 255 arguments.");
        }

        let paren = self.consume(&RightParen, "Expect ')' after function arguments.")?;

        Ok(Expr::Call {
            callee: expr.into(),
            paren: paren.into(),
            arguments,
        })
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
            Identifier => Expr::Variable(VariableExpr::new(candidate.into())),
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
                operator: operator.into(),
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

    fn check(&mut self, kind: &TokenKind) -> bool {
        match self.peek().map(|t| t.kind) {
            Some(actual) => &actual == kind,
            None => false,
        }
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
