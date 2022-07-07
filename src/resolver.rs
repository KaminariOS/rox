use crate::expr::{Expr, Jump, Stmt};
use crate::parser::StaticError;
use crate::scanner::Token;
use crate::Interpreter;
use std::collections::HashMap;

pub struct Resolver<'b, 'c: 'b> {
    interpreter: &'c mut Interpreter,
    scopes: Vec<HashMap<&'b str, bool>>,
}

impl<'b, 'c> Resolver<'b, 'c> {
    pub fn new(interpreter: &'c mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: vec![],
        }
    }
    pub fn resolve_statements(&mut self, statements: &'b [Stmt]) -> Result<(), StaticError> {
        for statement in statements {
            self.resolve_statement(statement)?;
        }
        Ok(())
    }

    pub fn resolve_statement(&mut self, statement: &'b Stmt) -> Result<(), StaticError> {
        match statement {
            Stmt::Block { statements } => {
                self.begin_scope();
                self.resolve_statements(statements)?;
                self.end_scope();
            }
            Stmt::Var { name, initializer } => {
                self.declare(name);
                if let Some(init) = initializer {
                    self.resolve_expr(init)?;
                }
                self.define(name);
            }
            Stmt::Function { name, params, body } => {
                self.declare(name);
                self.define(name);
                self.resolve_function(params, body)?;
            }
            Stmt::Expression(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_statement(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_statement(else_branch)?;
                }
            }
            Stmt::Print(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::Control(Jump::ReturnExpr { value, .. }) => {
                if let Some(expr) = value {
                    self.resolve_expr(expr)?;
                }
            }
            Stmt::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve_statement(body)?;
            }
            _ => {}
        };
        Ok(())
    }
    fn declare(&mut self, name: &'b Token) {
        if let Some(last) = self.scopes.last_mut() {
            last.insert(&name.lexeme, false);
        }
    }

    fn define(&mut self, name: &'b Token) {
        if let Some(last) = self.scopes.last_mut() {
            if let Some(entry) = last.get_mut(&name.lexeme as &str) {
                *entry = true;
            }
        }
    }

    fn resolve_expr(&mut self, expr: &'b Expr) -> Result<(), StaticError> {
        match expr {
            Expr::Variable { name, id } => {
                if self
                    .scopes
                    .last()
                    .and_then(|x| x.get(&name.lexeme as &str))
                    .filter(|&&x| !x)
                    .is_some()
                {
                    StaticError::new(
                        name.clone(),
                        "Can't read local variable in its own initializer",
                    )?;
                }
                self.resolve_local(*id, &name.lexeme);
            }
            Expr::Assign { name, value, id } => {
                self.resolve_expr(value)?;
                self.resolve_local(*id, &name.lexeme);
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Call { callee, args, .. } => {
                self.resolve_expr(callee)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
            }
            Expr::Grouping(expr) => {
                self.resolve_expr(expr)?;
            }
            Expr::Logical { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Unary { right, .. } => {
                self.resolve_expr(right)?;
            }
            _ => {}
        };
        Ok(())
    }

    fn resolve_function(
        &mut self,
        params: &'b [Token],
        body: &'b [Stmt],
    ) -> Result<(), StaticError> {
        self.begin_scope();
        for param in params {
            self.declare(param);
            self.define(param);
        }
        self.resolve_statements(body)?;
        self.end_scope();
        Ok(())
    }

    fn resolve_local(&mut self, id: usize, name: &str) {
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(name) {
                self.interpreter.resolve(id, self.scopes.len() - 1 - i);
                return;
            }
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}
