use translate::codegen::Codegen;
use winnow::{Located, Parser};

mod args;
mod syntax;
mod translate;

fn main() {
    let args = {
        use clap::Parser;
        args::Args::parse()
    };
    let source_content = std::fs::read_to_string(args.source).unwrap();
    let input = Located::new(source_content.as_str());
    let output = syntax::parse_program.parse(input).unwrap();
    let c = Codegen::translate(&output);
    print!("{c}");
}
