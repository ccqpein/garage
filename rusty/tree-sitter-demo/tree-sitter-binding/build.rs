use std::path::PathBuf;

fn main() {
    let dir: PathBuf = ["..", "tree-sitter-go", "src"].iter().collect();
    println!("dir: {:?}", dir);
    cc::Build::new()
        .include(&dir)
        .file(dir.join("parser.c"))
        //.file(dir.join("scanner.c"))
        .compile("tree-sitter-go");
}
