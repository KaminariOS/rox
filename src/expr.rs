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
    Variable {
        name: Token,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
}

pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
    Block {
        statements: Vec<Stmt>,
    },
    If {
        condition: Expr,
        thenBranch: Box<Stmt>,
        elseBranch: Option<Box<Stmt>>,
    },
    Control(Jump),
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
}

#[derive(Clone, Copy)]
pub enum Jump {
    Break,
    Continue,
}
