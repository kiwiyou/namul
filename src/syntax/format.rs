use winnow::{
    ascii::multispace1,
    combinator::{alt, delimited, opt, preceded, repeat},
    token::take_till,
    Located, PResult, Parser,
};

use crate::syntax::TokenKind;

use super::{parse_usize, Token};

#[derive(Debug, Clone)]
pub struct FormatString {
    pub fragment: Vec<FormatFragment>,
}

#[derive(Debug, Clone)]
pub enum FormatFragment {
    Literal(String),
    Ident(Token),
    Placeholder(Option<usize>),
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
                parse_format_ident.map(FormatFragment::Ident),
                parse_format_placeholder.map(FormatFragment::Placeholder),
            )),
        ),
        '`',
    )
    .map(|fragment| FormatString { fragment })
    .parse_next(s)
}

fn parse_format_literal<'a>(s: &mut Located<&'a str>) -> PResult<&'a str> {
    take_till(1.., ['`', '\\', '$']).parse_next(s)
}

fn parse_format_escape(s: &mut Located<&str>) -> PResult<char> {
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

fn parse_format_ident(s: &mut Located<&str>) -> PResult<Token> {
    preceded('$', TokenKind::Identifier).parse_next(s)
}

fn parse_escaped_whitespace<'a>(input: &mut Located<&'a str>) -> PResult<&'a str> {
    preceded('\\', multispace1).parse_next(input)
}

fn parse_format_placeholder(s: &mut Located<&str>) -> PResult<Option<usize>> {
    preceded('$', opt(parse_usize)).parse_next(s)
}
