use crate::expr::{Expr, Jump, Stmt};
use crate::parser::{CallableType, StaticError};
use crate::scanner::Token;
use crate::Interpreter;
use std::collections::HashMap;

pub struct Resolver<'b, 'c: 'b> {
    interpreter: &'c mut Interpreter,
    scopes: Vec<HashMap<&'b str, bool>>,
    current_function: Option<CallableType>,
    current_class: Option<ClassType>,
    in_loop: bool,
}

enum ClassType {
    Class,
    Subclass,
}

impl<'b, 'c> Resolver<'b, 'c> {
    pub fn new(interpreter: &'c mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: vec![],
            current_function: None,
            in_loop: false,
            current_class: None,
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
                if self
                    .scopes
                    .last()
                    .and_then(|x| x.get(&name.lexeme as &str))
                    .is_some()
                {
                    StaticError::new(
                        name.clone(),
                        "Already a variable with this name in this scope.",
                    )?;
                }
                self.declare(name);
                if let Some(init) = initializer {
                    self.resolve_expr(init)?;
                }
                self.define(name);
            }
            Stmt::Function { name, params, body } => {
                self.declare(name);
                self.define(name);
                self.resolve_function(params, body, Some(CallableType::Function))?;
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
            Stmt::Control(jump) => {
                match jump {
                    Jump::ReturnExpr { value, keyword } => {
                        if self.current_function.is_none() {
                            StaticError::new(keyword.clone(), "Can't return from top-level code.")?;
                        }
                        if let Some(expr) = value {
                            self.resolve_expr(expr)?;
                        }
                    }
                    Jump::Break { keyword } | Jump::Continue { keyword } => {
                        if !self.in_loop {
                            StaticError::new(
                                keyword.clone(),
                                "Can't break or continue when not in loop.",
                            )?;
                        }
                    }
                    _ => panic!(),
                };
            }
            Stmt::While { condition, body } => {
                self.resolve_expr(condition)?;
                let old = self.in_loop;
                self.in_loop = true;
                self.resolve_statement(body)?;
                self.in_loop = old;
            }
            Stmt::Class {
                name,
                methods,
                superclass,
            } => {
                let pre_class = self.current_class.take();
                self.current_class = Some(ClassType::Class);
                self.declare(name);
                self.define(name);
                if let Some(expr) = superclass {
                    self.current_class = Some(ClassType::Subclass);
                    self.begin_scope();
                    self.scopes.last_mut().map(|x| x.insert("super", true));
                    self.resolve_expr(expr)?;
                }
                self.begin_scope();
                self.scopes.last_mut().map(|x| x.insert("this", true));
                self.resolve_statements(methods)?;
                self.end_scope();
                if superclass.is_some() {
                    self.end_scope();
                }
                self.current_class = pre_class;
            }
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
            Expr::Get { object, .. } => {
                self.resolve_expr(object)?;
            }
            Expr::Set { value, object, .. } => {
                self.resolve_expr(value)?;
                self.resolve_expr(object)?;
            }
            Expr::This { keyword, id } => {
                if self.current_class.is_none() {
                    StaticError::new(keyword.clone(), "Can't use 'this' outside of a class")?
                }
                self.resolve_local(*id, &*keyword.lexeme);
            }
            Expr::Super { keyword, id, .. } => {
                match self.current_class.as_ref().ok_or(StaticError::new_err(
                    keyword.clone(),
                    "Can't use 'super' outside of a class.",
                ))? {
                    ClassType::Class => StaticError::new(
                        keyword.clone(),
                        "Can't use 'super' in a class with no superclass.",
                    )?,
                    ClassType::Subclass => self.resolve_local(*id, &keyword.lexeme),
                }
            }
            _ => {}
        };
        Ok(())
    }

    fn resolve_function(
        &mut self,
        params: &'b [Token],
        body: &'b [Stmt],
        function_type: Option<CallableType>,
    ) -> Result<(), StaticError> {
        let enclosing = self.current_function.take();
        self.current_function = function_type;
        self.begin_scope();
        for param in params {
            self.declare(param);
            self.define(param);
        }
        self.resolve_statements(body)?;
        self.end_scope();
        self.current_function = enclosing;
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
