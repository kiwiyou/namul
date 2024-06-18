use std::ops::Range;

use winnow::{
    ascii::multispace1,
    combinator::{alt, delimited, preceded, repeat},
    token::take_till,
    Located, PResult, Parser,
};

use crate::syntax::TokenKind;

use super::Token;

#[derive(Debug, Clone)]
pub struct FormatString {
    pub fragment: Vec<FormatFragment>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum FormatFragment {
    Literal(String),
    Ident { ident: Token, span: Range<usize> },
}

pub fn parse_format_string(s: &mut Located<&str>) -> PResult<FormatString> {
    #[derive(Debug, Clone)]
    enum LiteralFragment<'s> {
        Literal(&'s str),
        Escape(char),
        Whitespace,
    }
    delimited(
        '`',
        repeat(
            0..,
            alt((
                repeat(
                    1..,
                    alt((
                        parse_format_literal.map(LiteralFragment::Literal),
                        parse_format_escape.map(LiteralFragment::Escape),
                        parse_escaped_whitespace.value(LiteralFragment::Whitespace),
                    )),
                )
                .fold(String::new, |mut string, fragment| {
                    match fragment {
                        LiteralFragment::Literal(literal) => string.push_str(literal),
                        LiteralFragment::Escape(escape) => string.push(escape),
                        LiteralFragment::Whitespace => {}
                    }
                    string
                })
                .map(FormatFragment::Literal),
                parse_format_ident
                    .with_span()
                    .map(|(ident, span)| FormatFragment::Ident { ident, span }),
            )),
        ),
        '`',
    )
    .with_span()
    .map(|(fragment, span)| FormatString { fragment, span })
    .parse_next(s)
}

pub fn parse_format_literal<'a>(s: &mut Located<&'a str>) -> PResult<&'a str> {
    take_till(1.., ['`', '\\', '$']).parse_next(s)
}

pub fn parse_format_escape(s: &mut Located<&str>) -> PResult<char> {
    preceded(
        '\\',
        alt((
            'n'.value('\n'),
            'r'.value('\r'),
            't'.value('\t'),
            'b'.value('\u{08}'),
            'f'.value('\u{0C}'),
            '\\'.value('\\'),
            '/'.value('/'),
            '`'.value('`'),
            '"'.value('"'),
            '$'.value('$'),
        )),
    )
    .parse_next(s)
}

pub fn parse_format_ident(s: &mut Located<&str>) -> PResult<Token> {
    preceded('$', TokenKind::Identifier).parse_next(s)
}

pub fn parse_escaped_whitespace<'a>(input: &mut Located<&'a str>) -> PResult<&'a str> {
    preceded('\\', multispace1).parse_next(input)
}
