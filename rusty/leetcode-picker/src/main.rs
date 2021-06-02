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
                    println!("{}", Quiz::get_randomly(None)?);

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
                println!("{}", Quiz::get_randomly(None)?)
            }
        }
        false => {}
    }

    //let rep = Quiz::get_by_name(commandline_args.name())?;
    //println!("{}", rep.quiz_description()?);
    Ok(())
}
