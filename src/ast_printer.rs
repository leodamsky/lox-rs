use crate::Expr;

pub(crate) fn print_ast(expr: &Expr) -> String {
    match expr {
        Expr::Ternary {
            left,
            left_operator,
            mid,
            right_operator,
            right,
        } => parenthesize(format!(
            "{}{} {} {} {}",
            left_operator.lexeme,
            right_operator.lexeme,
            print_ast(left),
            print_ast(mid),
            print_ast(right),
        )),
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
        Expr::Literal(literal) => literal.to_string(),
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
    use crate::{Literal, Token, TokenKind};

    use super::*;

    #[test]
    fn test_print_ast() {
        let expr = Expr::Binary {
            left: Expr::Unary {
                operator: Token {
                    kind: TokenKind::Minus,
                    lexeme: '-'.to_string(),
                    literal: None,
                    line: 1,
                },
                right: Expr::Literal(Literal::Number(123.)).into(),
            }
            .into(),
            operator: Token {
                kind: TokenKind::Star,
                lexeme: '*'.to_string(),
                literal: None,
                line: 1,
            },
            right: Expr::Literal(Literal::Number(45.67)).into(),
        };

        let ast = print_ast(&expr);

        println!("{}", ast);
    }
}
