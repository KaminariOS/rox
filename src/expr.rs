use crate::interpreter::Type;
use crate::scanner::{Literal, Token};

#[derive(Clone)]
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
        id: usize,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
        id: usize,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        args: Vec<Expr>,
    },
    Get {
        object: Box<Expr>,
        name: Token,
    },
    Set {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },
    This {
        keyword: Token,
        id: usize,
    },
    Super {
        keyword: Token,
        method: Token,
        id: usize,
    },
}

#[derive(Clone)]
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
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Control(Jump),
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
    },
    Class {
        name: Token,
        methods: Vec<Stmt>,
        superclass: Option<Expr>,
    },
}

#[derive(Clone)]
pub enum Jump {
    Break { keyword: Token },
    Continue { keyword: Token },
    ReturnExpr { keyword: Token, value: Option<Expr> },
    ReturnValue { keyword: Token, value: Type },
}
