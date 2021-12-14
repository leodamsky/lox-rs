use crate::{Expr, Literal};

fn reverse_polish(expr: &Expr) -> String {
    match expr {
        Expr::Binary {
            left,
            operator,
            right,
        } => format!(
            "{} {} {}",
            reverse_polish(left),
            reverse_polish(right),
            operator.lexeme
        ),
        Expr::Grouping(expr) => reverse_polish(expr),
        Expr::Literal(literal) => match literal {
            None => "nil".to_string(),
            Some(literal) => literal.to_string(),
        },
        Expr::Unary { operator, right } => format!("{} {}", reverse_polish(right), operator.lexeme),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Literal, Token, TokenType};

    #[test]
    fn test_reverse_polish() {
        let expr = Expr::Binary {
            left: Expr::Grouping(
                Expr::Binary {
                    left: Expr::Literal(Some(Literal::Number(1f64))).into(),
                    operator: Token {
                        kind: TokenType::Plus,
                        lexeme: "+".to_string(),
                        literal: None,
                        line: 1,
                    },
                    right: Expr::Literal(Some(Literal::Number(2f64))).into(),
                }
                .into(),
            )
            .into(),
            operator: Token {
                kind: TokenType::Star,
                lexeme: "*".to_string(),
                literal: None,
                line: 1,
            },
            right: Expr::Grouping(
                Expr::Binary {
                    left: Expr::Literal(Some(Literal::Number(4f64))).into(),
                    operator: Token {
                        kind: TokenType::Minus,
                        lexeme: "-".to_string(),
                        literal: None,
                        line: 1,
                    },
                    right: Expr::Literal(Some(Literal::Number(3f64))).into(),
                }
                .into(),
            )
            .into(),
        };

        let result = reverse_polish(&expr);

        assert_eq!(result, "1 2 + 4 3 - *");
    }
}
