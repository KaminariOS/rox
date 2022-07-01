use crate::expr::Expr;

pub fn print_ast(expr: Box<Expr>) -> String {
    match *expr {
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            format!("({} {} {})", operator, print_ast(left), print_ast(right))
        }
        Expr::Grouping(expression) => {
            format!("(group {})", print_ast(expression))
        }
        Expr::LiteralNode(val) => {
            format!("{}", val)
        }
        Expr::Unary { operator, right } => {
            format!("({} {})", operator, print_ast(right))
        }
    }
}

#[cfg(test)]
#[test]
fn test_ast_printer() {
    use crate::expr::Expr::*;
    use crate::scanner::*;
    let exp = Binary {
        left: Box::new(Unary {
            operator: Token {
                token_type: TokenType::MINUS,
                lexeme: "-".to_owned(),
                line: 1,
            },
            right: Box::new(LiteralNode(Literal::Number(123f64))),
        }),
        operator: Token {
            token_type: TokenType::STAR,
            lexeme: "*".to_owned(),
            line: 1,
        },
        right: Box::new(Grouping(Box::new(LiteralNode(Literal::Number(45.67))))),
    };
    println!("{}", print_ast(Box::new(exp)));
}
