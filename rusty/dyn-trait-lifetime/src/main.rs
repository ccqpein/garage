use std::collections::HashMap;

trait A {}

struct App {
    table: HashMap<String, Box<dyn A>>,
}

impl App {
    fn register(&mut self, app: impl A + 'static) {
        self.table.insert(String::from("a"), Box::new(app));
    }
}

// failed
// impl App {
//     fn register<'s, 'a: 's>(&'s mut self, app: impl A + 'a) {
//         self.table.insert(String::from("a"), Box::new(app));
//     }
// }

//----------------------------------
struct Bpp<'b> {
    table: HashMap<String, Box<dyn A + 'b>>,
}

impl<'s, 'b: 's> Bpp<'b> {
    fn register(&'s mut self, app: impl A + 'b) {
        self.table.insert(String::from("a"), Box::new(app));
    }
}

fn main() {
    println!("Hello, world!");
}
