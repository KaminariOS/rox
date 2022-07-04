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

impl StaticError {
    fn new<T>(token: Token, msg: &str) -> Result<T, Self> {
        Err(Self {
            token,
            msg: msg.to_string(),
        })
    }
}

impl Display for StaticError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "StaticError: {} {:?}", self.msg, self.token)
    }
}

impl Error for StaticError {}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    repl: bool,
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>, repl: bool) -> Self {
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
            repl,
        }
    }
    pub(crate) fn parse(&mut self) -> Result<Vec<Stmt>, StaticError> {
        let mut stmts = vec![];
        while !self.is_at_end() {
            stmts.push(self.declaration(false)?);
        }
        Ok(stmts)
    }

    fn statement(&mut self, in_loop: bool) -> Result<Stmt, StaticError> {
        if self.match_(vec![FOR]) {
            self.for_statement()
        } else if self.match_(vec![IF]) {
            self.if_statement(in_loop)
        } else if self.match_(vec![PRINT]) {
            self.print_statement()
        } else if self.match_(vec![WHILE]) {
            self.while_statement()
        } else if self.match_(vec![LeftBrace]) {
            Ok(Stmt::Block {
                statements: self.block(in_loop)?,
            })
        } else if self.match_(vec![BREAK, CONTINUE]) {
            self.jump_statement(in_loop)
        } else {
            self.expression_statement()
        }
    }

    fn jump_statement(&mut self, in_loop: bool) -> Result<Stmt, StaticError> {
        let token = self.previous();
        if !in_loop {
            StaticError::new(
                token.clone(),
                &format!("{} statement can only exist in loop", token.lexeme),
            )?
        }
        let res = match &token.token_type {
            BREAK => Jump::Break,
            CONTINUE => Jump::Continue,
            _ => StaticError::new(token.clone(), "Invalid jump statement")?,
        };
        self.consume(SEMICOLON, &format!("Expect ';' after {}", token.lexeme))?;
        Ok(Stmt::Control(res))
    }

    fn for_statement(&mut self) -> Result<Stmt, StaticError> {
        self.consume(LeftParen, "Expect '(' after 'for'.");
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
        let mut body = self.statement(true)?;
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
        let body = Box::new(self.statement(true)?);
        Ok(Stmt::While { condition, body })
    }

    fn if_statement(&mut self, in_loop: bool) -> Result<Stmt, StaticError> {
        self.consume(LeftParen, "Expect '(' after 'if'.")?;
        let expr = self.expression()?;
        self.consume(RightParen, "Expect ')' after 'if' condition.")?;
        let thenBranch = Box::new(self.statement(in_loop)?);
        let elseBranch = if self.match_(vec![ELSE]) {
            Some(Box::new(self.statement(in_loop)?))
        } else {
            None
        };
        Ok(Stmt::If {
            condition: *expr,
            thenBranch,
            elseBranch,
        })
    }

    fn block(&mut self, in_loop: bool) -> Result<Vec<Stmt>, StaticError> {
        let mut statements = vec![];
        while !self.check(RightBrace) {
            statements.push(self.declaration(in_loop)?);
        }
        self.consume(RightBrace, "Expect '}' after block")?;
        Ok(statements)
    }

    fn declaration(&mut self, in_loop: bool) -> Result<Stmt, StaticError> {
        if self.match_(vec![VAR]) {
            self.var_declaration()
        } else {
            self.statement(in_loop)
        }
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
        self.primary()
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
            IDENTIFIER => Expr::Variable {
                name: self.peek().clone(),
            },
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
            match &*expr {
                Expr::Variable { name } => {
                    return Ok(Box::new(Expr::Assign {
                        name: name.clone(),
                        value,
                    }))
                }
                _ => StaticError::new(equals.clone(), "Invalid assignment target")?,
            }
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
