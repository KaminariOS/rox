use crate::scanner::{Literal, Token};

pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    LiteralNode(Literal),
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
}

pub enum Stmt {
    Expression(Expr),
    Print(Expr),
}
