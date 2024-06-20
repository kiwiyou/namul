use winnow::{
    combinator::{alt, delimited, opt, preceded, separated},
    seq, Located, PResult, Parser,
};

use super::{parse_usize, Token, TokenKind};

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
    Array(Array),
}

pub fn parse_type(s: &mut Located<&str>) -> PResult<Type> {
    alt((
        parse_type_path.map(Type::Path),
        parse_tuple.map(Type::Tuple),
        parse_array.map(Type::Array),
    ))
    .parse_next(s)
}

fn parse_tuple(s: &mut Located<&str>) -> PResult<Vec<Type>> {
    delimited(
        TokenKind::PunctLeftParenthesis,
        separated(
            1..,
            preceded(opt(TokenKind::White), parse_type),
            (opt(TokenKind::White), TokenKind::PunctComma),
        ),
        (
            opt(TokenKind::White),
            opt(TokenKind::PunctComma),
            opt(TokenKind::White),
            TokenKind::PunctRightParenthesis,
        ),
    )
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct Array {
    pub element: Box<Type>,
    pub len: usize,
}

fn parse_array(s: &mut Located<&str>) -> PResult<Array> {
    seq!(
        _: TokenKind::PunctLeftSquareBracket,
        _: opt(TokenKind::White),
        parse_type,
        _: opt(TokenKind::White),
        _: TokenKind::PunctSemicolon,
        _: opt(TokenKind::White),
        parse_usize,
        _: opt(TokenKind::White),
        _: TokenKind::PunctRightSquareBracket,
    )
    .map(|(element, len)| Array {
        element: Box::new(element),
        len,
    })
    .parse_next(s)
}
