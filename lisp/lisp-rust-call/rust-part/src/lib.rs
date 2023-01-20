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

/// add the tcp app
mod mod2 {
    use std::error::Error;
    use tokio::io::AsyncWriteExt;
    use tokio::net::TcpStream;
    use tokio::runtime::Runtime;

    #[no_mangle]
    pub extern "C" fn init_runtime() -> *mut Runtime {
        Box::into_raw(Box::new(Runtime::new().unwrap()))
    }

    #[no_mangle]
    pub extern "C" fn _safe_connect(
        rt_ptr: *mut Runtime,
        safe_ptr: *mut Safe,
        bootstrap_contact: *const c_char,
    ) {
        assert!(!safe_ptr.is_null());
        assert!(!rt_ptr.is_null());

        let bootstrap_contact = unsafe { CStr::from_ptr(bootstrap_contact) };
        let mut bootstrap_contacts = BootstrapConfig::default();
        bootstrap_contacts.insert(
            bootstrap_contact
                .parse()
                .expect("Invalid bootstrap address"),
        );

        unsafe {
            let _safe = &mut *safe_ptr;
            let rt = &mut *rt_ptr;
            rt.block_on(_safe.connect(None, None, Some(bootstrap_contacts)))
                .unwrap();
        }
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
