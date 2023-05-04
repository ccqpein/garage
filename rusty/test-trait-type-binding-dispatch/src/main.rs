//use std::collections::HashMap;

trait AppInput<T> {
    fn input(&self) -> T;
}

trait App<T>: AppInput<T> {
    //fn empty();
}

struct AppA {}
struct AppAInput {}

impl AppInput<AppAInput> for AppA {
    fn input(&self) -> AppAInput {
        AppAInput {}
    }
}

struct AppB {}
struct AppBInput {}

impl AppInput<AppBInput> for AppB {
    fn input(&self) -> AppBInput {
        AppBInput {}
    }
}

//
// test the struct dyn to diff types
//

trait B0 {
    fn output(&self) -> String {
        String::from("B0")
    }
}

trait B1 {
    fn output(&self) -> String {
        String::from("B1")
    }
}

trait B2 {
    fn output(&self) -> String {
        String::from("B2")
    }
}

impl B0 for AppB {
    fn output(&self) -> String {
        String::from("B0")
    }
}

impl B1 for AppB {
    fn output(&self) -> String {
        String::from("B1")
    }
}

fn main() {
    let b = AppB {};
    println!("{}", <AppB as B0>::output(&b));
    println!("{}", <AppB as B1>::output(&b));
}
