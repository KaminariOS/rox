use crate::function::{Callable, LoxFunction};
use crate::interpreter::Type;
use crate::scanner::Token;
use crate::types::{create_shared, Shared};
use crate::{Interpreter, RuntimeError};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub struct Class {
    name: String,
    methods: HashMap<String, Type>,
    superclass: Option<Shared<Class>>,
}

impl Class {
    pub fn new(
        name: &str,
        methods: HashMap<String, Type>,
        superclass: Option<Shared<Class>>,
    ) -> Shared<Self> {
        create_shared(Self {
            name: name.to_string(),
            methods,
            superclass,
        })
    }
    pub(crate) fn find_method(
        &self,
        name: &str,
        self_clone: Option<Shared<Instance>>,
    ) -> Option<Rc<dyn Callable>> {
        if let Some(Type::Function(method)) = self.methods.get(name) {
            Some(method.bind(self_clone))
        } else if let Some(sup) = &self.superclass {
            sup.borrow().find_method(name, self_clone)
        } else {
            None
        }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub struct ClassInstancing {
    pub(crate) class: Shared<Class>,
}

impl Callable for ClassInstancing {
    fn arity(&self) -> usize {
        self.class
            .borrow()
            .find_method("init", None)
            .map(|x| x.arity())
            .unwrap_or(0)
    }

    fn call(&self, interpreter: &mut Interpreter, args: &[Type]) -> Result<Type, RuntimeError> {
        let instance = Instance::new(self.class.clone());
        if let Some(init) = instance
            .borrow()
            .class
            .borrow()
            .find_method("init", Some(instance.clone()))
        {
            init.call(interpreter, args)?;
        }
        Ok(Type::Object(instance))
    }

    fn name(&self) -> String {
        self.class.borrow().name.clone()
    }

    fn bind(&self, _instance: Option<Shared<Instance>>) -> Rc<LoxFunction> {
        panic!("Not for this one")
    }
}

pub struct Instance {
    class: Shared<Class>,
    fields: HashMap<String, Type>,
}

impl Instance {
    pub fn new(class: Shared<Class>) -> Shared<Self> {
        create_shared(Self {
            class,
            fields: HashMap::default(),
        })
    }
    pub fn get(&self, name: &Token, self_clone: Shared<Self>) -> Result<Type, RuntimeError> {
        let val = if let Some(val) = self.fields.get(&name.lexeme as &str) {
            val.clone()
        } else if let Some(method) = self
            .class
            .borrow()
            .find_method(&name.lexeme as &str, Some(self_clone))
        {
            Type::Function(method)
        } else {
            RuntimeError::new(
                name.clone(),
                &format!("Undefined property '{}'.", &name.lexeme),
            )?
        };
        Ok(val)
    }

    pub fn set(&mut self, name: &Token, val: Type) {
        self.fields.insert((&name.lexeme).to_string(), val);
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class.borrow().to_string())
    }
}
