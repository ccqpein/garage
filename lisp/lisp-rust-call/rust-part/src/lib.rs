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

/// tokio runtime to cffi
/*mod mod2 {
    use async_ffi::{FfiFuture, FutureExt};
    use std::error::Error;
    use tokio::io::AsyncWriteExt;
    use tokio::net::TcpStream;
    use tokio::runtime::Handle;
    use tokio::runtime::Runtime;

    #[no_mangle]
    pub extern "C" fn init_runtime() -> *mut Runtime {
        Box::into_raw(Box::new(Runtime::new().unwrap()))
    }

    // #[no_mangle]
    // pub extern "C" fn _safe_connect(
    //     rt_ptr: *mut Runtime,
    //     safe_ptr: *mut Safe,
    //     bootstrap_contact: *const c_char,
    // ) {
    //     assert!(!safe_ptr.is_null());
    //     assert!(!rt_ptr.is_null());

    //     let bootstrap_contact = unsafe { CStr::from_ptr(bootstrap_contact) };
    //     let mut bootstrap_contacts = BootstrapConfig::default();
    //     bootstrap_contacts.insert(
    //         bootstrap_contact
    //             .parse()
    //             .expect("Invalid bootstrap address"),
    //     );

    //     unsafe {
    //         let _safe = &mut *safe_ptr;
    //         let rt = &mut *rt_ptr;
    //         rt.block_on(_safe.connect(None, None, Some(bootstrap_contacts)))
    //             .unwrap();
    //     }
    // }
}*/

mod mod2 {
    use std::net::{TcpListener, TcpStream};

    #[no_mangle]
    pub extern "C" fn input_func_as_parameter<F>(f: *const F, n: i32)
    where
        F: Fn(i32) -> i32,
    {
        // if I don't use the point, I don't know the type in common lisp
        unsafe { println!("hello: {}", (*f)(n)) }
    }

    #[no_mangle]
    pub extern "C" fn open_tcp_server<F>(handle: F) -> std::io::Result<()>
    where
        F: Fn(TcpStream) -> std::io::Result<()>,
    {
        let listener = TcpListener::bind("127.0.0.1:80")?;

        // accept connections and process them serially
        for stream in listener.incoming() {
            match stream {
                Ok(ss) => {
                    handle(ss);
                }
                Err(e) => {
                    println!("err: {}", e.to_string())
                }
            }
        }
        Ok(())
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
