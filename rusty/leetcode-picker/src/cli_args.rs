use clap::Clap;

/// command line arguments
#[derive(Default, Clap, Debug)]
#[clap(version = "0.1")]
pub struct Args {
    // quiz id
    // #[clap(long)]
    // id: usize,
    /// quiz name
    #[clap(long = "name")]
    quiz_name: Option<String>,

    /// random pick one
    #[clap(long)]
    random: bool,

    /// interact or not
    #[clap(short)]
    interact: bool,
}

impl Args {
    pub fn name(&self) -> &str {
        self.quiz_name.as_ref().unwrap().as_str()
    }

    pub fn if_random(&self) -> bool {
        self.random
    }

    pub fn if_interact(&self) -> bool {
        self.interact
    }
}
