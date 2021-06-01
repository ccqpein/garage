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
    quiz_name: String,
}

impl Args {
    pub fn name(&self) -> &str {
        self.quiz_name.as_str()
    }
}
