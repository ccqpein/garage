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

fn main() {}
