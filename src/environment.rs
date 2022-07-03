use crate::interpreter::Type;
use crate::scanner::Token;
use crate::types::{create_shared, Shared};
use crate::RuntimeError;
use std::collections::HashMap;

pub struct Environment {
    values: HashMap<String, Option<Type>>,
    enclosing: Option<Shared<Environment>>,
}

impl Environment {
    pub fn new(enclosing: Option<Shared<Environment>>) -> Shared<Environment> {
        create_shared(Environment {
            values: HashMap::default(),
            enclosing,
        })
    }
    pub fn define(&mut self, name: String, val: Option<Type>) {
        self.values.insert(name, val);
    }
    pub fn assign(&mut self, name: &Token, val: Type) -> Result<(), RuntimeError> {
        let id = &*name.lexeme;
        if let Some(value) = self.values.get_mut(id) {
            *value = Some(val);
            Ok(())
        } else if let Some(enclosing) = self.enclosing.as_mut() {
            enclosing.borrow_mut().assign(name, val)?;
            Ok(())
        } else {
            RuntimeError::new(name.clone(), "Undefined variable")
        }
    }
    pub fn get(&self, name: &Token) -> Option<Option<Type>> {
        let id = &*name.lexeme;
        let res = self.values.get(id);
        if res.is_some() {
            res.map(|x| x.as_ref().map(|x| Self::pass_val(x)))
        } else if let Some(enclosing) = self.enclosing.as_ref() {
            enclosing.borrow().get(name)
        } else {
            None
        }
    }

    fn pass_val(val: &Type) -> Type {
        match val {
            Type::Primitive(l) => Type::Primitive(l.clone()),
            val => val.clone(),
        }
    }
}
