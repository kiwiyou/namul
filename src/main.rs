use translate::Translator;
use winnow::Parser;

mod args;
mod syntax;
mod translate;

fn main() {
    let args = {
        use clap::Parser;
        args::Args::parse()
    };
    let source_content = std::fs::read_to_string(args.source).unwrap();
    let input = source_content.as_str();
    let output = syntax::parse_program.parse(input).unwrap();
    let c = Translator::translate(&output);
    print!("{c}");
}
