use winnow::{
    ascii::multispace0,
    combinator::{alt, delimited, opt, preceded, separated},
    PResult, Parser,
};

use super::path::{parse_ident, Identifier};

#[derive(Debug, Clone)]
pub struct TypePath {
    pub ident: Identifier,
}

pub fn parse_type_path<'s>(input: &mut &'s str) -> PResult<TypePath> {
    parse_ident
        .map(|ident| TypePath { ident })
        .parse_next(input)
}

#[derive(Debug, Clone)]
pub enum Type {
    Path(TypePath),
    Tuple(Vec<Self>),
}

pub fn parse_type<'s>(input: &mut &'s str) -> PResult<Type> {
    alt((
        parse_type_path.map(Type::Path),
        parse_tuple.map(Type::Tuple),
    ))
    .parse_next(input)
}

pub fn parse_tuple<'s>(input: &mut &'s str) -> PResult<Vec<Type>> {
    delimited(
        '(',
        separated(0.., preceded(multispace0, parse_type), ','),
        (opt((multispace0, ',')), multispace0, ')'),
    )
    .parse_next(input)
}
