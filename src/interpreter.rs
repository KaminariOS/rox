use crate::expr::{Expr, Stmt};
use crate::scanner::{Literal, Token, TokenType};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

pub enum Type {
    Primitive(Literal),
    Object,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(l) => write!(f, "{}", l),
            Self::Object => write!(f, "Object"),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    token: Token,
    msg: String,
}

impl RuntimeError {
    fn new<T>(token: Token, msg: &str) -> Result<T, Self> {
        Err(Self {
            token,
            msg: msg.to_string(),
        })
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?}", self.msg, self.token)
    }
}

impl Error for RuntimeError {}

pub struct Interpreter {}

impl Interpreter {
    pub fn visit(expr: &Expr) -> Result<Type, RuntimeError> {
        let res = match expr {
            Expr::LiteralNode(literal) => Type::Primitive(literal.clone()),
            Expr::Grouping(expr) => Self::visit(expr)?,
            Expr::Unary { operator, right } => {
                let token_type = &operator.token_type;
                let right_val = Self::visit(right)?;
                match (token_type, right_val) {
                    (TokenType::MINUS, Type::Primitive(Literal::Number(num))) => {
                        Type::Primitive(Literal::Number(-num))
                    }
                    (TokenType::BANG, right_val) => {
                        let bo = match right_val {
                            Type::Object => true,
                            Type::Primitive(literal) => is_truthy(literal),
                        };
                        Type::Primitive(Literal::Boolean(!bo))
                    }
                    _ => RuntimeError::new(operator.clone(), "Invalid unary operator")?,
                }
            }
            Expr::Binary {
                left,
                right,
                operator,
            } => {
                let token_type = &operator.token_type;
                let left = Self::visit(left)?;
                let right = Self::visit(right)?;
                match [left, right] {
                    lr if *token_type == TokenType::EqualEqual
                        || *token_type == TokenType::BangEqual =>
                    {
                        let res = match token_type {
                            TokenType::EqualEqual => is_equal(lr),
                            _ => !is_equal(lr),
                        };
                        Type::Primitive(Literal::Boolean(res))
                    }
                    [Type::Primitive(Literal::Number(left_val)), Type::Primitive(Literal::Number(right_val))] =>
                    {
                        let res = match token_type {
                            TokenType::MINUS => Ok(Literal::Number(left_val - right_val)),
                            TokenType::SLASH => Ok(Literal::Number(left_val / right_val)),
                            TokenType::STAR => Ok(Literal::Number(left_val * right_val)),
                            TokenType::PLUS => Ok(Literal::Number(left_val + right_val)),
                            TokenType::GREATER => Ok(Literal::Boolean(left_val > right_val)),
                            TokenType::LESS => Ok(Literal::Boolean(left_val < right_val)),
                            TokenType::LessEqual => Ok(Literal::Boolean(left_val <= right_val)),
                            TokenType::GreaterEqual => Ok(Literal::Boolean(left_val >= right_val)),
                            _ => RuntimeError::new(
                                operator.clone(),
                                "Unsupported binary operator for Number",
                            ),
                        };
                        res.and_then(|x| Ok(Type::Primitive(x)))?
                    }
                    [Type::Primitive(Literal::String(sl)), Type::Primitive(Literal::String(sr))] => {
                        let res = match token_type {
                            TokenType::PLUS => Literal::String(Arc::new(sl.to_string() + &*sr)),
                            _ => RuntimeError::new(
                                operator.clone(),
                                "Unsupported binary operator for String",
                            )?,
                        };
                        Type::Primitive(res)
                    }
                    _ => RuntimeError::new(operator.clone(), "Unsupported binary operator")?,
                }
            }
        };
        Ok(res)
    }
    pub fn interpret_stmt(statements: &Vec<Stmt>) -> Result<(), RuntimeError> {
        for statement in statements {
            match statement {
                Stmt::Expression(expr) => {
                    let val = Self::interpret(expr)?;
                }
                Stmt::Print(expr) => {
                    let val = Self::interpret(expr)?;
                    println!("{}", val);
                }
            }
        }
        Ok(())
    }
    pub fn interpret(expr: &Expr) -> Result<Type, RuntimeError> {
        let res = Self::visit(expr)?;

        Ok(res)
    }
}

fn is_truthy(val: Literal) -> bool {
    match val {
        Literal::NIL | Literal::Boolean(false) => false,
        _ => true,
    }
}

fn is_equal([left_type, right_type]: [Type; 2]) -> bool {
    match [left_type, right_type] {
        [Type::Primitive(left_val), Type::Primitive(right_val)] => match [left_val, right_val] {
            [Literal::Number(l), Literal::Number(r)] => l == r,
            [Literal::Boolean(l), Literal::Boolean(r)] => l == r,
            [Literal::String(l), Literal::String(r)] => l == r,
            [Literal::NIL, Literal::NIL] => true,
            _ => false,
        },
        [Type::Object, Type::Object] => true,
        _ => false,
    }
}
