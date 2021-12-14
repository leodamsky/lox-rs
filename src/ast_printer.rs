use crate::{Expr, Literal};

pub(crate) fn print_ast(expr: &Expr) -> String {
    match expr {
        Expr::Binary {
            left,
            operator,
            right,
        } => parenthesize(format!(
            "{} {} {}",
            operator.lexeme,
            print_ast(left),
            print_ast(right)
        )),
        Expr::Grouping(expr) => parenthesize(format!("group {}", print_ast(expr))),
        Expr::Literal(literal) => match literal {
            None => "nil".to_string(),
            Some(literal) => literal.to_string(),
        },
        Expr::Unary { operator, right } => {
            parenthesize(format!("{} {}", operator.lexeme, print_ast(right)))
        }
    }
}

fn parenthesize(mut value: String) -> String {
    value.insert(0, '(');
    value.push(')');
    value
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Token, TokenType};

    #[test]
    fn test_print_ast() {
        let expr = Expr::Binary {
            left: Expr::Unary {
                operator: Token {
                    kind: TokenType::Minus,
                    lexeme: '-'.to_string(),
                    literal: None,
                    line: 1,
                },
                right: Expr::Literal(Some(Literal::Number(123.))).into(),
            }
            .into(),
            operator: Token {
                kind: TokenType::Star,
                lexeme: '*'.to_string(),
                literal: None,
                line: 1,
            },
            right: Expr::Literal(Some(Literal::Number(45.67))).into(),
        };

        let ast = print_ast(&expr);

        println!("{}", ast);
    }
}
