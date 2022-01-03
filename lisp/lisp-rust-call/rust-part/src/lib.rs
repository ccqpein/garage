mod mod0 {
    fn hello_mod0() -> String {
        "hello mod0".to_string()
    }
}

mod mod1 {
    use std::ffi::CString;

    #[no_mangle]
    pub extern "C" fn hello_mod1() -> CString {
        CString::new("hello mod1").expect("in mod1 failed")
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
