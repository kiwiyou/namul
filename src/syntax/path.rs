use winnow::{
    token::{one_of, take_while},
    PResult, Parser,
};

#[derive(Debug, Clone)]
pub enum Path {
    Simple(Identifier),
}

pub fn parse_path<'s>(input: &mut &'s str) -> PResult<Path> {
    parse_ident.map(Path::Simple).parse_next(input)
}

pub type Identifier = String;

pub fn parse_ident<'s>(input: &mut &'s str) -> PResult<Identifier> {
    (
        one_of(|c| matches!(c, '_' | 'A'..='Z' | 'a'..='z')),
        take_while(0.., |c| matches!(c, '_'| '0'..='9' | 'A'..='Z' | 'a'..='z')),
    )
        .map(|(first, rest)| {
            let mut ident = String::new();
            ident.push(first);
            ident.push_str(rest);
            ident
        })
        .parse_next(input)
}
