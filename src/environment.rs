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

    pub fn get_at(&self, distance: usize, name: &str) -> Option<Option<Type>> {
        if distance == 0 {
            return self.values.get(name).map(|x| x.as_ref().map(|x| x.clone()));
        }
        if let Some(env) = self.ancestor(distance) {
            env.borrow()
                .values
                .get(name)
                .map(|x| x.as_ref().map(|x| x.clone()))
        } else {
            None
        }
    }
    fn ancestor(&self, distance: usize) -> Option<Shared<Environment>> {
        let mut env = self.enclosing.clone();
        for _ in 1..distance {
            if let Some(enc) = env {
                env = enc.borrow().enclosing.clone();
            } else {
                return None;
            }
        }
        env
    }

    pub fn assign_at(
        &mut self,
        distance: usize,
        val: Type,
        name: &Token,
    ) -> Result<(), RuntimeError> {
        if distance == 0 {
            self.assign(name, val.clone())?;
        } else if let Some(env) = self.ancestor(distance) {
            env.borrow_mut().assign(name, val)?;
        } else {
            RuntimeError::new(name.clone(), "Undefined variable")?;
        }
        Ok(())
    }
}
