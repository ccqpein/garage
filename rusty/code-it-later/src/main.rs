use clap::Clap;
use code_it_later::*;

fn main() {
    //let args = config::Opts::parse();
    let args = config::Args::parse();
    println!("{:?}", args);
}
