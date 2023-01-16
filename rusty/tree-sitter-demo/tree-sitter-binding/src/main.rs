use std::{
    error::Error,
    fs::File,
    io::{BufReader, Read},
};

use tree_sitter::{Language, Parser};

extern "C" {
    fn tree_sitter_go() -> Language;
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_go() };

    parser.set_language(language);

    let mut f = BufReader::new(
        File::open("../../../gopher/generics-playground/generics-type.go")
            .map_err(|e| e.to_string())?,
    );

    let mut code = String::new();
    f.read_to_string(&mut code);

    //println!("source code:\n{:?}", code);

    let tree = parser.parse(code, None).unwrap();

    //println!("{:?}", tree.root_node().to_sexp());

    let mut cursor = tree.root_node().walk();
    let children = tree.root_node().children(&mut cursor);
    for c in children {
        println!("{:?}", c);
    }

    println!("===============");

    let mut cursor1 = tree.root_node().child(2).unwrap().walk();
    println!("{:?}", cursor1.node());
    cursor1.goto_first_child();
    println!("{:?}", cursor1.node());
    cursor1.goto_next_sibling();
    println!("{:?}", cursor1.node());

    println!("===============");
    // can re-use the cursor already operated upper
    let children1 = tree.root_node().child(2).unwrap().children(&mut cursor1);
    for c in children1 {
        println!("{:?}", c);
    }

    // loop {
    //     println!(
    //         "-----------------\n{:?}\n{:?}",
    //         cursor.node(),
    //         cursor.node().to_sexp()
    //     );
    //     if !cursor.goto_next_sibling() {
    //         println!("break");
    //         break;
    //     }
    // }

    Ok(())
}
