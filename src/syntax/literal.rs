use winnow::{combinator::alt, Located, PResult, Parser};

use super::{Token, TokenKind};

#[derive(Debug, Clone)]
pub enum Literal {
    Decimal(DecimalLiteral),
    Bool(BoolLiteral),
}

pub fn parse_literal<'s>(s: &mut Located<&str>) -> PResult<Literal> {
    alt((
        parse_bool_literal.map(Literal::Bool),
        parse_decimal_literal.map(Literal::Decimal),
    ))
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct DecimalLiteral(pub Token);

pub fn parse_decimal_literal(s: &mut Located<&str>) -> PResult<DecimalLiteral> {
    TokenKind::LiteralDecimal.map(DecimalLiteral).parse_next(s)
}

#[derive(Debug, Clone)]
pub struct BoolLiteral(pub Token);

pub fn parse_bool_literal(s: &mut Located<&str>) -> PResult<BoolLiteral> {
    alt((TokenKind::KeywordTrue, TokenKind::KeywordFalse))
        .map(BoolLiteral)
        .parse_next(s)
}
