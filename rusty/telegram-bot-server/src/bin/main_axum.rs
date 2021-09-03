include!("same_part");

fn main() {
    // expand main macro
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(async {})
}
