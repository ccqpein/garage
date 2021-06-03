use clap::Clap;
use leetcode_picker::*;
use question::{Answer, Question};

fn main() -> Result<(), String> {
    let commandline_args = cli_args::Args::parse();
    dbg!(&commandline_args);
    match commandline_args.if_random() {
        true => {
            if commandline_args.if_interact() {
                loop {
                    println!("{}", Quiz::get_randomly(commandline_args.level())?);

                    // ask
                    let a = Question::new("Is this good?")
                        .yes_no()
                        .until_acceptable()
                        .ask()
                        .unwrap();

                    if Answer::YES == a {
                        break;
                    }
                }
            } else {
                println!("{}", Quiz::get_randomly(commandline_args.level())?)
            }
        }
        false => {
            // try id first
            if let Some(ref id) = commandline_args.quiz_id() {
                println!("{}", Quiz::get_by_id(*id)?);
                return Ok(());
            }

            // try name then
            if let Some(ref name) = commandline_args.name() {
                println!("{}", Quiz::get_by_name(name)?);
                return Ok(());
            }

            println!("If it is not random, need more info. Check -h")
        }
    }

    Ok(())
}
