use clap::Parser;
use lisp_rpc_rust_generator::*;
use std::fs::File;
use std::io;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long, value_name = "spec-file")]
    input_file: PathBuf,

    #[arg(short, long, value_name = "lib-name")]
    lib_name: String,
}

fn parse_spec_file(file: File) -> io::Result<()> {
    let mut parser: lisp_rpc_rust_parser::Parser = Default::default();
    let exprs = parser
        .parse_root(file)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    for expr in &exprs {
        if DefRPC::if_def_rpc_expr(expr) {
            //:= rpc generate
        } else if DefMsg::if_def_msg_expr(expr) {
            //:= sg generate
        }

        return Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "Unsupported expr",
        ));
    }

    Ok(())
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    let input_path = &args.input_file;

    if !input_path.exists() {
        eprintln!("Error: Input file does not exist at {:?}", input_path);
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            "Input file not found",
        ));
    }
    if !input_path.is_file() {
        eprintln!("Error: Path {:?} is not a file.", input_path);
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Path is not a file",
        ));
    }

    let file = File::open(input_path)?;
    parse_spec_file(file)?;

    //dbg!(exprs);
    Ok(())
}
