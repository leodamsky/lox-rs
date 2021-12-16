use std::iter::Peekable;
use std::vec::IntoIter;

use TokenKind::{
    Bang, BangEqual, Class, EOF, EqualEqual, False, For, Fun, Greater, GreaterEqual, If,
    LeftParen, Less, LessEqual, Minus, Nil, Number, Plus, Print, Return, Semicolon, Slash, Star, True,
    Var, While,
};

use crate::{Expr, Literal, Token, TokenKind};
use crate::parser::util::error;
use crate::TokenKind::RightParen;

struct ParseError {}

pub(crate) struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub(crate) fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub(crate) fn parse(mut self) -> Option<Expr> {
        self.expression().ok()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.ternary()
    }

    fn ternary(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        if let Some(q_operator) = self.try_consume(&TokenKind::Question) {
            let mid = self.ternary()?;
            let c_operator =
                self.consume(&TokenKind::Colon, "Expect ':' in a ternary expression.")?;
            let right = self.ternary()?;
            expr = Expr::Ternary {
                left: expr.into(),
                left_operator: q_operator,
                mid: mid.into(),
                right_operator: c_operator,
                right: right.into(),
            }
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
        let candidate = self
            .advance()
            .ok_or_else(|| error(self.force_peek(), "Expect expression, got end of input."))?;

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
            _ => return Err(error(&candidate, "Expect expression.")),
        };

        Ok(expr)
    }

    fn parse_left_associative<T>(
        &mut self,
        kinds: &[TokenKind],
        mut handle: T,
    ) -> Result<Expr, ParseError>
    where
        T: FnMut(&mut Parser) -> Result<Expr, ParseError>,
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
            Err(util::error(self.force_peek(), message))
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
}

mod util {
    use crate::parser::ParseError;
    use crate::Token;

    pub(super) fn error(token: &Token, message: impl AsRef<str>) -> ParseError {
        crate::syntax_error(token, message);
        ParseError {}
    }
}
