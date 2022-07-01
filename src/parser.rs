use crate::expr::{Expr, Stmt};
use crate::scanner::{error, Literal, Token, TokenType, TokenType::*};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    pub(crate) fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = vec![];
        while !self.is_at_end() {
            stmts.push(self.statement());
        }
        stmts
    }
    fn statement(&mut self) -> Stmt {
        if self.match_(vec![PRINT]) {
            self.printStatement()
        } else {
            self.expressionStatement()
        }
    }
    fn printStatement(&mut self) -> Stmt {
        let expr = self.expression();
        self.consume(SEMICOLON, "Expect ';' after value");
        Stmt::Print(*expr)
    }
    fn expressionStatement(&mut self) -> Stmt {
        let expr = self.expression();
        self.consume(SEMICOLON, "Expect ';' after expression");
        Stmt::Expression(*expr)
    }
    fn expression(&mut self) -> Box<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Box<Expr> {
        let mut expr = self.comparison();
        while self.match_(vec![BangEqual, EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison();
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }
        expr
    }

    fn comparison(&mut self) -> Box<Expr> {
        let mut expr = self.term();

        while self.match_(vec![GREATER, GreaterEqual, LESS, LessEqual]) {
            let operator = self.previous().clone();
            let right = self.term();
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }
        expr
    }

    fn term(&mut self) -> Box<Expr> {
        let mut expr = self.factor();

        while self.match_(vec![MINUS, PLUS]) {
            let operator = self.previous().clone();
            let right = self.factor();
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }
        expr
    }

    fn factor(&mut self) -> Box<Expr> {
        let mut expr = self.unary();

        while self.match_(vec![SLASH, STAR]) {
            let operator = self.previous().clone();
            let right = self.unary();
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }
        expr
    }

    fn unary(&mut self) -> Box<Expr> {
        if self.match_(vec![BANG, MINUS]) {
            let operator = self.previous().clone();
            let right = self.unary();
            return Box::new(Expr::Unary { operator, right });
        }
        self.primary()
    }

    fn primary(&mut self) -> Box<Expr> {
        let mut paren = false;
        let expr = match &self.peek().token_type {
            FALSE => Expr::LiteralNode(Literal::Boolean(false)),
            TRUE => Expr::LiteralNode(Literal::Boolean(true)),
            NIL => Expr::LiteralNode(Literal::NIL),
            STRING(s) | NUMBER(s) => Expr::LiteralNode(s.clone()),
            LeftParen => {
                self.advance();
                let exp = Expr::Grouping(self.expression());
                self.consume(RightParen, "Expect ')' after expression");
                paren = true;
                exp
            }
            _ => error(self.peek(), "Expect expression"),
        };
        if !paren {
            self.advance();
        }
        Box::new(expr)
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
    fn consume(&mut self, type_: TokenType, msg: &str) -> &Token {
        if self.check(type_) {
            return self.advance();
        }
        error(self.peek(), msg)
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
