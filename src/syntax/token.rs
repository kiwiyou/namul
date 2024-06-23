use std::ops::Range;

use winnow::{
    ascii::{digit1, multispace1},
    combinator::{alt, opt},
    error::ContextError,
    token::{literal, one_of, take_while},
    Located, PResult, Parser,
};

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub content: String,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    White,
    Identifier,
    LiteralDecimal,
    KeywordRep,
    KeywordIf,
    KeywordElse,
    KeywordTrue,
    KeywordFalse,
    KeywordWhile,
    KeywordFn,
    KeywordReturn,
    KeywordAs,
    PunctComma,
    PunctSemicolon,
    PunctVerticalLine,
    PunctPlusSign,
    PunctHyphenMinus,
    PunctAsterisk,
    PunctSolidus,
    PunctPercentSign,
    PunctLeftParenthesis,
    PunctRightParenthesis,
    PunctLeftCurlyBracket,
    PunctRightCurlyBracket,
    PunctEqualsSign,
    PunctLessThanSign,
    PunctGreaterThanSign,
    PunctExclamationMarkEqualsSign,
    PunctEqualsSignEqualsSign,
    PunctLessThanSignEqualsSign,
    PunctGreaterThanSignEqualsSign,
    PunctAmpersandAmpersand,
    PunctVerticalLineVerticalLine,
    PunctQuestionMark,
    PunctColon,
    PunctPlusSignEqualsSign,
    PunctHyphenMinusEqualsSign,
    PunctAsteriskEqualsSign,
    PunctSolidusEqualsSign,
    PunctPercentSignEqualsSign,
    PunctLowLine,
    PunctLeftSquareBracket,
    PunctRightSquareBracket,
    PunctAmpersand,
    PunctVerticalLineEqualsSign,
    PunctAmpersandEqualsSign,
    PunctFullStopFullStop,
    PunctFullStopFullStopEqualsSign,
}

fn token<T: Into<String>>(kind: TokenKind) -> impl Fn((T, Range<usize>)) -> Token {
    move |(content, span)| Token {
        kind: kind.clone(),
        content: content.into(),
        span,
    }
}

impl Parser<Located<&str>, Token, ContextError> for TokenKind {
    fn parse_next(&mut self, s: &mut Located<&str>) -> PResult<Token, ContextError> {
        parse_token
            .verify(|token| token.kind == self.clone())
            .parse_next(s)
    }
}

pub fn parse_token(s: &mut Located<&str>) -> PResult<Token> {
    alt((
        parse_white,
        parse_literal_decimal,
        parse_keyword,
        parse_identifier,
        parse_punct,
    ))
    .parse_next(s)
}

pub fn parse_punct(s: &mut Located<&str>) -> PResult<Token> {
    alt([
        literal("..=")
            .with_span()
            .map(token(TokenKind::PunctFullStopFullStopEqualsSign)),
        literal("..")
            .with_span()
            .map(token(TokenKind::PunctFullStopFullStop)),
        literal("+=")
            .with_span()
            .map(token(TokenKind::PunctPlusSignEqualsSign)),
        literal("-=")
            .with_span()
            .map(token(TokenKind::PunctHyphenMinusEqualsSign)),
        literal("*=")
            .with_span()
            .map(token(TokenKind::PunctAsteriskEqualsSign)),
        literal("/=")
            .with_span()
            .map(token(TokenKind::PunctSolidusEqualsSign)),
        literal("%=")
            .with_span()
            .map(token(TokenKind::PunctPercentSignEqualsSign)),
        literal("|=")
            .with_span()
            .map(token(TokenKind::PunctVerticalLineEqualsSign)),
        literal("&=")
            .with_span()
            .map(token(TokenKind::PunctAmpersandEqualsSign)),
        literal("&&")
            .with_span()
            .map(token(TokenKind::PunctAmpersandAmpersand)),
        literal("||")
            .with_span()
            .map(token(TokenKind::PunctVerticalLineVerticalLine)),
        literal("!=")
            .with_span()
            .map(token(TokenKind::PunctExclamationMarkEqualsSign)),
        literal("==")
            .with_span()
            .map(token(TokenKind::PunctEqualsSignEqualsSign)),
        literal("<=")
            .with_span()
            .map(token(TokenKind::PunctLessThanSignEqualsSign)),
        literal(">=")
            .with_span()
            .map(token(TokenKind::PunctGreaterThanSignEqualsSign)),
        literal("[")
            .with_span()
            .map(token(TokenKind::PunctLeftSquareBracket)),
        literal("]")
            .with_span()
            .map(token(TokenKind::PunctRightSquareBracket)),
        literal("_").with_span().map(token(TokenKind::PunctLowLine)),
        literal(":").with_span().map(token(TokenKind::PunctColon)),
        literal("?")
            .with_span()
            .map(token(TokenKind::PunctQuestionMark)),
        literal("=")
            .with_span()
            .map(token(TokenKind::PunctEqualsSign)),
        literal("<")
            .with_span()
            .map(token(TokenKind::PunctLessThanSign)),
        literal(">")
            .with_span()
            .map(token(TokenKind::PunctGreaterThanSign)),
        literal(",").with_span().map(token(TokenKind::PunctComma)),
        literal(";")
            .with_span()
            .map(token(TokenKind::PunctSemicolon)),
        literal("&")
            .with_span()
            .map(token(TokenKind::PunctAmpersand)),
        literal("|")
            .with_span()
            .map(token(TokenKind::PunctVerticalLine)),
        literal("+")
            .with_span()
            .map(token(TokenKind::PunctPlusSign)),
        literal("-")
            .with_span()
            .map(token(TokenKind::PunctHyphenMinus)),
        literal("*")
            .with_span()
            .map(token(TokenKind::PunctAsterisk)),
        literal("/").with_span().map(token(TokenKind::PunctSolidus)),
        literal("%")
            .with_span()
            .map(token(TokenKind::PunctPercentSign)),
        literal("(")
            .with_span()
            .map(token(TokenKind::PunctLeftParenthesis)),
        literal(")")
            .with_span()
            .map(token(TokenKind::PunctRightParenthesis)),
        literal("{")
            .with_span()
            .map(token(TokenKind::PunctLeftCurlyBracket)),
        literal("}")
            .with_span()
            .map(token(TokenKind::PunctRightCurlyBracket)),
    ])
    .parse_next(s)
}

pub fn parse_literal_decimal(s: &mut Located<&str>) -> PResult<Token> {
    (opt(one_of(['-', '+'])), digit1)
        .map(|(sign, digits)| {
            let mut literal = String::new();
            if let Some(sign) = sign {
                literal.push(sign);
            }
            literal.push_str(digits);
            literal
        })
        .with_span()
        .map(token(TokenKind::LiteralDecimal))
        .parse_next(s)
}

pub fn parse_identifier(s: &mut Located<&str>) -> PResult<Token> {
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
        .with_span()
        .map(token(TokenKind::Identifier))
        .parse_next(s)
}

pub fn parse_white(s: &mut Located<&str>) -> PResult<Token> {
    multispace1
        .with_span()
        .map(token(TokenKind::White))
        .parse_next(s)
}

pub fn parse_keyword(s: &mut Located<&str>) -> PResult<Token> {
    alt((
        literal("if").with_span().map(token(TokenKind::KeywordIf)),
        literal("as").with_span().map(token(TokenKind::KeywordAs)),
        literal("else")
            .with_span()
            .map(token(TokenKind::KeywordElse)),
        literal("while")
            .with_span()
            .map(token(TokenKind::KeywordWhile)),
        literal("rep").with_span().map(token(TokenKind::KeywordRep)),
        literal("fn").with_span().map(token(TokenKind::KeywordFn)),
        literal("return")
            .with_span()
            .map(token(TokenKind::KeywordReturn)),
        literal("true")
            .with_span()
            .map(token(TokenKind::KeywordTrue)),
        literal("false")
            .with_span()
            .map(token(TokenKind::KeywordFalse)),
    ))
    .parse_next(s)
}
