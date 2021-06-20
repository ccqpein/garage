fn main() -> Result<(), Box<dyn std::error::Error>> {
    tonic_build::configure()
        .out_dir("./OUTPUT") // this folder has to be exsit
        .compile(&["../protocols/hello.proto"], &["../protocols"])?;
    Ok(())
}
