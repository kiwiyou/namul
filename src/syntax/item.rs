use winnow::{
    combinator::{alt, delimited, opt, preceded, separated},
    Located, PResult, Parser,
};

use super::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct TypePath {
    pub ident: Token,
}

pub fn parse_type_path(input: &mut Located<&str>) -> PResult<TypePath> {
    TokenKind::Identifier
        .map(|ident| TypePath { ident })
        .parse_next(input)
}

#[derive(Debug, Clone)]
pub enum Type {
    Path(TypePath),
    Tuple(Vec<Self>),
}

pub fn parse_type(input: &mut Located<&str>) -> PResult<Type> {
    alt((
        parse_type_path.map(Type::Path),
        parse_tuple.map(Type::Tuple),
    ))
    .parse_next(input)
}

pub fn parse_tuple(input: &mut Located<&str>) -> PResult<Vec<Type>> {
    delimited(
        TokenKind::PunctLeftParenthesis,
        separated(
            0..,
            preceded(opt(TokenKind::White), parse_type),
            TokenKind::PunctComma,
        ),
        (
            opt((opt(TokenKind::White), TokenKind::PunctComma)),
            opt(TokenKind::White),
            TokenKind::PunctRightParenthesis,
        ),
    )
    .parse_next(input)
}
