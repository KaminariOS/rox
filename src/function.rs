use crate::environment::Environment;
use crate::expr::{Jump, Stmt};
use crate::interpreter::Type;
use crate::scanner::{Literal, Token};
use crate::types::Shared;
use crate::{Interpreter, RuntimeError};
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Type>) -> Result<Type, RuntimeError>;
    fn name(&self) -> String;
}

impl Display for dyn Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name())
    }
}

pub struct Clock;

impl Callable for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: &mut Interpreter, _: &Vec<Type>) -> Result<Type, RuntimeError> {
        let start = SystemTime::now();
        let since_the_epoch = start
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards");
        Ok(Type::Primitive(Literal::Number(
            since_the_epoch.as_secs_f64(),
        )))
    }

    fn name(&self) -> String {
        "clock".to_string()
    }
}

pub struct LoxFunction {
    name: Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
    closure: Shared<Environment>,
}

impl LoxFunction {
    pub fn new(
        name: &Token,
        params: &Vec<Token>,
        body: &Vec<Stmt>,
        closure: Shared<Environment>,
    ) -> Rc<Self> {
        Rc::new(Self {
            name: name.clone(),
            params: params.iter().map(|x| x.clone()).collect(),
            body: body.iter().map(|x| x.clone()).collect(),
            closure,
        })
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Callable for LoxFunction {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Type>) -> Result<Type, RuntimeError> {
        let old_env = interpreter.environment.clone();
        let environment = Environment::new(Some(self.closure.clone()));
        {
            let mut mutable_borrow = environment.borrow_mut();
            for (i, param) in self.params.iter().enumerate() {
                mutable_borrow.define(param.to_string(), Some(args[i].clone()));
            }
        }
        interpreter.environment = environment;
        let res = if let Some(Jump::ReturnValue { value: val, .. }) =
            interpreter.interpret_stmts(&self.body)?
        {
            val
        } else {
            Type::Primitive(Literal::NIL)
        };
        interpreter.environment = old_env;
        Ok(res)
    }

    fn name(&self) -> String {
        self.name.to_string()
    }
}
