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

    /// quiz id
    #[clap(long = "id")]
    quiz_id: Option<u64>,

    /// random pick one
    #[clap(short, long)]
    random: bool,

    /// interact or not
    #[clap(short)]
    interact: bool,
}

impl Args {
    pub fn name(&self) -> &Option<String> {
        &self.quiz_name
    }

    pub fn if_random(&self) -> bool {
        self.random
    }

    pub fn if_interact(&self) -> bool {
        self.interact
    }

    pub fn quiz_id(&self) -> &Option<u64> {
        &self.quiz_id
    }
}
