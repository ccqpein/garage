use std::error::Error;

struct A {
    name: String,
}

impl A {
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }

    fn a(&self) -> Result<Vec<i32>, Box<dyn Error + '_>> {
        let aaa = Self::new(&self.name).a()?;
        Ok(vec![])
    }
}

// fn a(s: &str) -> Result<Vec<i32>, Box<dyn Error + '_>> {
//     Ok(vec![])
// }

// fn b(s: &str) -> Result<Vec<i32>, Box<dyn Error + '_>> {
//     let mut res = vec![];

//     res.append(&mut b("")?);
//     Ok(res)
// }

fn main() {
    println!("Hello, world!");
}
