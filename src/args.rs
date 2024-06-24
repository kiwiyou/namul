use clap::Parser;

#[derive(Parser, Debug)]
pub struct Args {
    pub source: String,
    #[clap(long)]
    pub no_libc: bool,
}
