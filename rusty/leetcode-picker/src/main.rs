use clap::Clap;
use leetcode_picker::*;

fn main() -> Result<(), String> {
    let commandline_args = cli_args::Args::parse();

    let rep = Quiz::get_by_name(commandline_args.name())?;
    println!("{}", rep.quiz_description()?);
    Ok(())
}
