use proc_macro_test::*;

// #[derive(DeriveTestYo)]
// struct A;

// #[derive(GiveMeFields, Debug)]
// struct B {
//     #[this]
//     a: String,

//     #[this]
//     b: i64,
// }

// #[normal_attribute(char, i32)]
// pub fn test_C() {}

#[derive(AutoImpl)]
enum C {
    #[to(char, i32)]
    C,

    #[to(usize, char)]
    D(char),

    E,
}

// below should be the upper macro generated

// trait CAble {
//     fn into_C_C(&self) -> Option<&dyn CC>;
//     fn into_C_D(&self) -> Option<&dyn CD>;
//     fn into_C_E(&self) -> Option<&dyn CE>;
// }

// impl CAble for char {
//     fn into_C_C(&self) -> Option<&dyn CC> {
//         Some(self)
//     }

//     fn into_C_D(&self) -> Option<&dyn CD> {
//         Some(self)
//     }

//     fn into_C_E(&self) -> Option<&dyn CE> {
//         None
//     }
// }

// impl CAble for i32 {
//     fn into_C_C(&self) -> Option<&dyn CC> {
//         Some(self)
//     }

//     fn into_C_D(&self) -> Option<&dyn CD> {
//         None
//     }

//     fn into_C_E(&self) -> Option<&dyn CE> {
//         None
//     }
// }

// impl CAble for usize {
//     fn into_C_C(&self) -> Option<&dyn CC> {
//         None
//     }

//     fn into_C_D(&self) -> Option<&dyn CD> {
//         Some(self)
//     }

//     fn into_C_E(&self) -> Option<&dyn CE> {
//         None
//     }
// }

// trait CC {}
// trait CD {}
// trait CE {}

// then manully implenment below
impl CC for char {}
impl CC for i32 {}
impl CD for char {}
impl CD for usize {}

fn main() {
    //println!("{}", A::hello_macros());
    //println!("{:?}", B::give_me_fields());
}
