use crate::expr::{Expr, Jump, Stmt};
use crate::scanner::{Literal, Token, TokenType, TokenType::*};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

#[derive(Debug)]
pub struct StaticError {
    token: Token,
    msg: String,
}

pub enum CallableType {
    Function,
    Method,
}

impl Display for CallableType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Function => "function",
            Self::Method => "Method",
        };
        write!(f, "{}", s)
    }
}

impl StaticError {
    pub fn new<T>(token: Token, msg: &str) -> Result<T, Self> {
        Err(Self::new_err(token, msg))
    }
    pub fn new_err(token: Token, msg: &str) -> Self {
        Self {
            token,
            msg: msg.to_string(),
        }
    }
}

impl Display for StaticError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "StaticError: {} {:?}", self.msg, self.token)
    }
}

impl Error for StaticError {}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    id: &'a mut usize,
}

impl<'a> Parser<'a> {
    fn get_id(&mut self) -> usize {
        let res = *self.id;
        *self.id += 1;
        res
    }
    pub fn new(mut tokens: Vec<Token>, repl: bool, id: &'a mut usize) -> Self {
        let len = tokens.len();
        if !tokens.is_empty()
            && repl
            && tokens
                .get(len - 2)
                .filter(|x| x.token_type != SEMICOLON && x.token_type != RightBrace)
                .is_some()
        {
            tokens.insert(
                len - 1,
                Token {
                    token_type: SEMICOLON,
                    lexeme: Arc::new(";".to_owned()),
                    line: 1,
                },
            );
            tokens.insert(
                0,
                Token {
                    token_type: PRINT,
                    lexeme: Arc::new("print".to_owned()),
                    line: 1,
                },
            );
        }
        Self {
            tokens,
            current: 0,
            id,
        }
    }
    pub(crate) fn parse(&mut self) -> Result<Vec<Stmt>, StaticError> {
        let mut stmts = vec![];
        while !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        Ok(stmts)
    }

    fn statement(&mut self) -> Result<Stmt, StaticError> {
        if self.match_(vec![FOR]) {
            self.for_statement()
        } else if self.match_(vec![IF]) {
            self.if_statement()
        } else if self.match_(vec![PRINT]) {
            self.print_statement()
        } else if self.match_(vec![WHILE]) {
            self.while_statement()
        } else if self.match_(vec![RETURN]) {
            self.return_statement()
        } else if self.match_(vec![LeftBrace]) {
            Ok(Stmt::Block {
                statements: self.block()?,
            })
        } else if self.match_(vec![BREAK, CONTINUE]) {
            self.jump_statement()
        } else {
            self.expression_statement()
        }
    }

    fn class_declaration(&mut self) -> Result<Stmt, StaticError> {
        let name = self.consume(IDENTIFIER, "Expect class name.")?.clone();
        let superclass = if self.match_(vec![LESS]) {
            let superclass_name = self.consume(IDENTIFIER, "Expect superclass name.")?.clone();
            if superclass_name.lexeme == name.lexeme {
                StaticError::new(superclass_name, "A class can't inherit from itself")?
            } else {
                Some(Expr::Variable {
                    name: superclass_name,
                    id: self.get_id(),
                })
            }
        } else {
            None
        };
        self.consume(LeftBrace, "Expect '{' before class body.")?;
        let mut methods = vec![];
        while !self.check(RightBrace) {
            methods.push(self.function(CallableType::Method)?);
        }
        self.consume(RightBrace, "Expect '}' after class body.")?;
        Ok(Stmt::Class {
            name,
            methods,
            superclass,
        })
    }

    fn return_statement(&mut self) -> Result<Stmt, StaticError> {
        let keyword = self.previous().clone();
        let value = if !self.check(SEMICOLON) {
            Some(*self.expression()?)
        } else {
            None
        };
        self.consume(SEMICOLON, "Expect ';' after return value.")?;
        Ok(Stmt::Control(Jump::ReturnExpr { keyword, value }))
    }

    fn jump_statement(&mut self) -> Result<Stmt, StaticError> {
        let token = self.previous().clone();
        self.consume(SEMICOLON, &format!("Expect ';' after {}", token.lexeme))?;
        let res = match &token.token_type {
            BREAK => Jump::Break { keyword: token },
            CONTINUE => Jump::Continue { keyword: token },
            _ => StaticError::new(token.clone(), "Invalid jump statement")?,
        };
        Ok(Stmt::Control(res))
    }

    fn for_statement(&mut self) -> Result<Stmt, StaticError> {
        self.consume(LeftParen, "Expect '(' after 'for'.")?;
        let init: Option<Stmt> = if self.match_(vec![SEMICOLON]) {
            None
        } else if self.match_(vec![VAR]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };
        let condition = if !self.check(SEMICOLON) {
            Some(*self.expression()?)
        } else {
            None
        };
        self.consume(SEMICOLON, "Expect ';' after loop condition")?;
        let increment = if !self.check(RightParen) {
            Some(*self.expression()?)
        } else {
            None
        };
        self.consume(RightParen, "Expect ')' after for clause.")?;
        let mut body = self.statement()?;
        if let Some(increment) = increment {
            body = Stmt::Block {
                statements: vec![body, Stmt::Expression(increment)],
            }
        }
        if let Some(condition) = condition {
            body = Stmt::While {
                condition,
                body: Box::new(body),
            };
        }
        if let Some(init) = init {
            body = Stmt::Block {
                statements: vec![init, body],
            }
        }
        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt, StaticError> {
        self.consume(LeftParen, "Expect '(' after 'while'.")?;
        let condition = *self.expression()?;
        self.consume(RightParen, "Expect ')' after 'while' condition")?;
        let body = Box::new(self.statement()?);
        Ok(Stmt::While { condition, body })
    }

    fn if_statement(&mut self) -> Result<Stmt, StaticError> {
        self.consume(LeftParen, "Expect '(' after 'if'.")?;
        let expr = self.expression()?;
        self.consume(RightParen, "Expect ')' after 'if' condition.")?;
        let then_branch = Box::new(self.statement()?);
        let else_branch = if self.match_(vec![ELSE]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        Ok(Stmt::If {
            condition: *expr,
            then_branch,
            else_branch,
        })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, StaticError> {
        let mut statements = vec![];
        while !self.check(RightBrace) {
            statements.push(self.declaration()?);
        }
        self.consume(RightBrace, "Expect '}' after block")?;
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, StaticError> {
        if self.match_(vec![FUN]) {
            self.function(CallableType::Function)
        } else if self.match_(vec![VAR]) {
            self.var_declaration()
        } else if self.match_(vec![CLASS]) {
            self.class_declaration()
        } else {
            self.statement()
        }
    }

    fn function(&mut self, kind: CallableType) -> Result<Stmt, StaticError> {
        let kind = kind.to_string();
        let name = self
            .consume(IDENTIFIER, &format!("Expect {} name", kind))?
            .clone();
        self.consume(LeftParen, &format!("Expect '(' after {} name", kind))?;
        let mut params = vec![];
        if !self.check(RightParen) {
            loop {
                if params.len() >= 255 {
                    StaticError::new(name.clone(), "Can't have more than 255 parameters")?;
                }
                params.push(self.consume(IDENTIFIER, "Expect parameter name.")?.clone());
                if !self.match_(vec![COMMA]) {
                    break;
                }
            }
        }
        self.consume(RightParen, "Expect ')' after parameters.")?;
        self.consume(LeftBrace, &format!("Expect '{{' before {} body.", kind))?;
        let body = self.block()?;
        Ok(Stmt::Function { name, params, body })
    }

    fn var_declaration(&mut self) -> Result<Stmt, StaticError> {
        let name = self.consume(IDENTIFIER, "Expect variable name.")?.clone();
        let initializer = if self.match_(vec![EQUAL]) {
            Some(*self.expression()?)
        } else {
            None
        };
        self.consume(SEMICOLON, "Expect ';' after variable declaration")?;
        Ok(Stmt::Var { name, initializer })
    }

    fn print_statement(&mut self) -> Result<Stmt, StaticError> {
        let expr = self.expression()?;
        self.consume(SEMICOLON, "Expect ';' after value")?;
        Ok(Stmt::Print(*expr))
    }

    fn expression_statement(&mut self) -> Result<Stmt, StaticError> {
        let expr = self.expression()?;
        self.consume(SEMICOLON, "Expect ';' after expression")?;
        Ok(Stmt::Expression(*expr))
    }

    fn expression(&mut self) -> Result<Box<Expr>, StaticError> {
        self.assignment()
    }

    fn equality(&mut self) -> Result<Box<Expr>, StaticError> {
        let mut expr = self.comparison()?;
        while self.match_(vec![BangEqual, EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Box<Expr>, StaticError> {
        let mut expr = self.term()?;

        while self.match_(vec![GREATER, GreaterEqual, LESS, LessEqual]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Box<Expr>, StaticError> {
        let mut expr = self.factor()?;

        while self.match_(vec![MINUS, PLUS]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Box<Expr>, StaticError> {
        let mut expr = self.unary()?;

        while self.match_(vec![SLASH, STAR]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Box<Expr>, StaticError> {
        if self.match_(vec![BANG, MINUS]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Box::new(Expr::Unary { operator, right }));
        }
        self.call()
    }

    fn call(&mut self) -> Result<Box<Expr>, StaticError> {
        let mut expr = *self.primary()?;
        loop {
            if self.match_(vec![LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_(vec![DOT]) {
                let name = self.consume(IDENTIFIER, "Expect property name after '.'.")?;
                expr = Expr::Get {
                    object: Box::new(expr),
                    name: name.clone(),
                };
            } else {
                break;
            }
        }
        Ok(Box::new(expr))
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, StaticError> {
        let mut args = vec![];
        if !self.check(RightParen) {
            loop {
                args.push(*self.expression()?);
                if args.len() >= 255 {
                    eprintln!("{} Can' t have more than 255 arguments", self.peek());
                    break;
                }
                if !self.match_(vec![COMMA]) {
                    break;
                }
            }
        }
        let paren = self
            .consume(RightParen, "Expect ')' after arguments")?
            .clone();
        Ok(Expr::Call {
            callee: Box::new(callee),
            paren,
            args,
        })
    }

    fn primary(&mut self) -> Result<Box<Expr>, StaticError> {
        let mut paren = false;
        let expr = match &self.peek().token_type {
            FALSE => Expr::LiteralNode(Literal::Boolean(false)),
            TRUE => Expr::LiteralNode(Literal::Boolean(true)),
            NIL => Expr::LiteralNode(Literal::NIL),
            STRING(s) | NUMBER(s) => Expr::LiteralNode(s.clone()),
            LeftParen => {
                self.advance();
                let exp = Expr::Grouping(self.expression()?);
                self.consume(RightParen, "Expect ')' after expression")?;
                paren = true;
                exp
            }
            IDENTIFIER => {
                let expr = Expr::Variable {
                    name: self.peek().clone(),
                    id: self.get_id(),
                };
                expr
            }
            THIS => {
                let expr = Expr::This {
                    keyword: self.peek().clone(),
                    id: self.get_id(),
                };
                expr
            }
            SUPER => {
                paren = true;
                let keyword = self.advance().clone();
                self.consume(DOT, "Expect '.' after 'super'.")?;
                let method = self
                    .consume(IDENTIFIER, "Expect superclass method name.")?
                    .clone();
                Expr::Super {
                    keyword,
                    method,
                    id: self.get_id(),
                }
            }
            _ => StaticError::new(self.peek().clone(), "Expect expression")?,
        };
        if !paren {
            self.advance();
        }
        Ok(Box::new(expr))
    }

    fn assignment(&mut self) -> Result<Box<Expr>, StaticError> {
        let expr = self.or()?;
        if self.match_(vec![EQUAL]) {
            let value = self.assignment()?;
            let equals = self.previous();
            let res = match *expr {
                Expr::Variable { name, id } => Expr::Assign {
                    name: name.clone(),
                    value,
                    id,
                },
                Expr::Get { object, name } => Expr::Set {
                    object,
                    value,
                    name,
                },
                _ => StaticError::new(equals.clone(), "Invalid assignment target")?,
            };
            return Ok(Box::new(res));
        }
        Ok(expr)
    }

    fn or(&mut self) -> Result<Box<Expr>, StaticError> {
        let mut expr = self.and()?;
        while self.match_(vec![OR]) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Box::new(Expr::Logical {
                left: expr,
                right,
                operator,
            });
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Box<Expr>, StaticError> {
        let mut expr = self.equality()?;
        while self.match_(vec![AND]) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Box::new(Expr::Logical {
                left: expr,
                operator,
                right,
            });
        }
        Ok(expr)
    }

    fn match_(&mut self, types: Vec<TokenType>) -> bool {
        for type_ in types {
            if self.check(type_) {
                self.advance();
                return true;
            }
        }
        false
    }
    fn consume(&mut self, type_: TokenType, msg: &str) -> Result<&Token, StaticError> {
        if self.check(type_) {
            Ok(self.advance())
        } else {
            StaticError::new(self.peek().clone(), msg)?
        }
    }
    fn check(&self, type_: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().token_type == type_
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        match &self.peek().token_type {
            EOF => true,
            _ => false,
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    #[allow(dead_code)]
    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if let SEMICOLON = self.previous().token_type {
                return;
            }
            match &self.peek().token_type {
                CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return,
                _ => {}
            }
            self.advance();
        }
    }
}
