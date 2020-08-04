use clap::Clap;
use code_it_later::*;

fn main() {
    //let args = config::Opts::parse();
    let args = config::Argvs::parse();
    println!("{:?}", args);
}
