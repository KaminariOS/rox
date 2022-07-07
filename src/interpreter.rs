use crate::environment::Environment;
use crate::expr::{Expr, Jump, Stmt};
use crate::function::{Callable, Clock, LoxFunction};
use crate::scanner::{Literal, Token, TokenType};
use crate::types::Shared;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::sync::Arc;

#[derive(Clone)]
pub enum Type {
    Primitive(Literal),
    Object,
    Function(Rc<dyn Callable>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(l) => write!(f, "{}", l),
            Self::Object => write!(f, "Object"),
            Self::Function(func) => write!(f, "Function {}", func),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    token: Token,
    msg: String,
}

impl RuntimeError {
    pub fn new<T>(token: Token, msg: &str) -> Result<T, Self> {
        Err(Self {
            token,
            msg: msg.to_string(),
        })
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Runtime Error: {} {:?}", self.msg, self.token)
    }
}

impl Error for RuntimeError {}

pub struct Interpreter {
    pub environment: Shared<Environment>,
    pub globals: Shared<Environment>,
    locals: HashMap<usize, usize>,
    pub(crate) id: usize,
}

impl Interpreter {
    pub fn resolve(&mut self, id: usize, dis: usize) {
        self.locals.insert(id, dis);
    }

    pub fn new() -> Self {
        let env = Environment::new(None);
        let clock = Clock;
        env.borrow_mut()
            .define(clock.name(), Some(Type::Function(Rc::new(clock))));
        Self {
            environment: env.clone(),
            globals: env,
            locals: HashMap::new(),
            id: 0,
        }
    }

    pub fn visit(&mut self, expr: &Expr) -> Result<Type, RuntimeError> {
        let res = match expr {
            Expr::LiteralNode(literal) => Type::Primitive(literal.clone()),
            Expr::Grouping(expr) => self.visit(expr)?,
            Expr::Unary { operator, right } => {
                let token_type = &operator.token_type;
                let right_val = self.visit(right)?;
                match (token_type, right_val) {
                    (TokenType::MINUS, Type::Primitive(Literal::Number(num))) => {
                        Type::Primitive(Literal::Number(-num))
                    }
                    (TokenType::BANG, right_val) => {
                        let bo = is_truthy(&right_val);
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
                let left = self.visit(left)?;
                let right = self.visit(right)?;
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
            Expr::Variable { name, id } => {
                let val = self
                    .lookup_variable(name, id)
                    .and_then(|x| x)
                    .unwrap_or(Type::Primitive(Literal::NIL));
                val
            }
            Expr::Assign { name, value, id } => {
                let val = self.visit(value)?;
                if let Some(dis) = self.locals.get(id) {
                    self.environment
                        .borrow_mut()
                        .assign_at(*dis, val.clone(), name)?;
                } else {
                    self.globals.borrow_mut().assign(name, val.clone())?;
                }
                val
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let val = self.visit(left)?;
                match operator.token_type {
                    TokenType::AND => {
                        if is_truthy(&val) {
                            self.visit(right)?
                        } else {
                            val
                        }
                    }
                    TokenType::OR => {
                        if is_truthy(&val) {
                            val
                        } else {
                            self.visit(right)?
                        }
                    }
                    _ => RuntimeError::new(operator.clone(), "Invalid operator")?,
                }
            }
            Expr::Call {
                callee,
                args,
                paren,
            } => {
                let callee = self.visit(callee)?;
                let token = Token {
                    token_type: TokenType::IDENTIFIER,
                    lexeme: Arc::new(callee.to_string()),
                    line: paren.line,
                };
                let func = if let Type::Function(callee) = callee {
                    callee
                } else {
                    RuntimeError::new(token.clone(), "Expect Callable")?
                };

                if func.arity() != args.len() {
                    RuntimeError::new(
                        token,
                        &format!("Expect {} arguments but got {}.", func.arity(), args.len()),
                    )?
                }
                let mut arguments = vec![];
                for i in args {
                    arguments.push(self.visit(i)?);
                }
                let old_env = self.environment.clone();
                func.call(self, &arguments)?
            }
        };
        Ok(res)
    }
    pub fn interpret_stmts(&mut self, statements: &[Stmt]) -> Result<Option<Jump>, RuntimeError> {
        for statement in statements {
            let res = self.interpret_stmt(statement)?;
            if res.is_some() {
                return Ok(res);
            }
        }
        Ok(None)
    }
    fn lookup_variable(&self, name: &Token, id: &usize) -> Option<Option<Type>> {
        if let Some(&distance) = self.locals.get(id) {
            self.environment.borrow().get_at(distance, &name.lexeme)
        } else {
            self.globals.borrow().get(name)
        }
    }

    pub fn interpret_stmt(&mut self, statement: &Stmt) -> Result<Option<Jump>, RuntimeError> {
        match statement {
            Stmt::Expression(expr) => {
                let _ = self.interpret(expr)?;
            }
            Stmt::Print(expr) => {
                let val = self.interpret(expr)?;
                println!("{}", val);
            }
            Stmt::Var { name, initializer } => {
                let value = if let Some(init) = initializer {
                    Some(self.interpret(init)?)
                } else {
                    None
                };
                self.environment
                    .borrow_mut()
                    .define(name.lexeme.to_string(), value);
            }
            Stmt::Block { statements } => {
                let res = self.execute_block(statements, self.environment.clone())?;
                if res.is_some() {
                    return Ok(res);
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let val = self.visit(condition)?;
                let res = if is_truthy(&val) {
                    self.interpret_stmt(then_branch)?
                } else if let Some(else_statement) = else_branch {
                    self.interpret_stmt(else_statement)?
                } else {
                    None
                };
                if res.is_some() {
                    return Ok(res);
                }
            }
            Stmt::While { condition, body } => {
                while is_truthy(&self.visit(condition)?) {
                    let res = self.interpret_stmt(body)?;
                    if let Some(j) = res {
                        match j {
                            Jump::Break => break,
                            Jump::Continue => continue,
                            re => return Ok(Some(re)),
                        }
                    }
                }
            }
            Stmt::Control(jump) => {
                let res = if let Jump::ReturnExpr { keyword, value } = jump {
                    let res = if let Some(expr) = value {
                        self.visit(expr)?
                    } else {
                        Type::Primitive(Literal::NIL)
                    };
                    Jump::ReturnValue {
                        keyword: keyword.clone(),
                        value: res,
                    }
                } else {
                    jump.clone()
                };
                return Ok(Some(res));
            }
            Stmt::Function { name, params, body } => {
                let func = LoxFunction::new(name, params, body, self.environment.clone());
                self.environment
                    .borrow_mut()
                    .define(func.name(), Some(Type::Function(func)));
            }
        }
        Ok(None)
    }
    pub fn execute_block(
        &mut self,
        statements: &Vec<Stmt>,
        previous: Shared<Environment>,
    ) -> Result<Option<Jump>, RuntimeError> {
        let env = Environment::new(Some(previous.clone()));
        self.environment = env;
        let res = self.interpret_stmts(statements)?;
        self.environment = previous;
        Ok(res)
    }
    pub fn interpret(&mut self, expr: &Expr) -> Result<Type, RuntimeError> {
        let res = self.visit(expr)?;

        Ok(res)
    }
}

fn is_truthy(val: &Type) -> bool {
    match val {
        Type::Primitive(l) => match l {
            Literal::NIL | Literal::Boolean(false) => false,
            _ => true,
        },
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
