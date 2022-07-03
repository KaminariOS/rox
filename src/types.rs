use std::cell::RefCell;
use std::rc::Rc;

pub type Shared<T> = Rc<RefCell<T>>;

pub fn create_shared<T>(val: T) -> Shared<T> {
    Rc::new(RefCell::new(val))
}
