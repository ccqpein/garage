extern "C" {
    fn tree_sitter_go() -> Language;
}

fn main() {
    let language = unsafe { tree_sitter_go() };
}
