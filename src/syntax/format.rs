use winnow::{
    ascii::multispace1,
    combinator::{alt, delimited, preceded, repeat},
    token::take_till,
    PResult, Parser,
};

use super::path::{parse_ident, Identifier};

#[derive(Debug, Clone)]
pub struct FormatString {
    pub fragment: Vec<FormatFragment>,
}

#[derive(Debug, Clone)]
pub enum FormatFragment {
    Literal(String),
    Ident(Identifier),
}

pub fn parse_format_string<'s>(input: &mut &'s str) -> PResult<FormatString> {
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
            )),
        ),
        '`',
    )
    .map(|fragment| FormatString { fragment })
    .parse_next(input)
}

pub fn parse_format_literal<'s>(input: &mut &'s str) -> PResult<&'s str> {
    take_till(1.., ['`', '\\', '$']).parse_next(input)
}

pub fn parse_format_escape<'s>(input: &mut &'s str) -> PResult<char> {
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
    .parse_next(input)
}

pub fn parse_format_ident<'s>(input: &mut &'s str) -> PResult<Identifier> {
    preceded('$', parse_ident).parse_next(input)
}

pub fn parse_escaped_whitespace<'s>(input: &mut &'s str) -> PResult<&'s str> {
    preceded('\\', multispace1).parse_next(input)
}
