pub mod map;

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    #[test]
    fn it_works() {
        let a: Rc<RefCell<i32>> = Rc::new(RefCell::new(1));
        let b = Rc::clone(&a);

        assert_eq!(Rc::as_ptr(&a), Rc::as_ptr(&b) as *const RefCell<i32>);
        assert_eq!(a.as_ptr(), b.as_ptr() as *mut i32);
    }
}
