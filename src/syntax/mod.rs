use std::ops::Range;

use stmt::{parse_statement, Statement};
use winnow::{
    combinator::{opt, preceded, repeat, terminated},
    Located, PResult, Parser,
};

pub mod expr;
pub mod format;
pub mod item;
pub mod literal;
pub mod path;
pub mod stmt;
pub mod token;

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
    BangEq,
    EqEq,
    LtEq,
    GtEq,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statement: Vec<Statement>,
}

pub fn parse_program(s: &mut Located<&str>) -> PResult<Program> {
    terminated(
        repeat(0.., preceded(opt(TokenKind::White), parse_statement)),
        opt(TokenKind::White),
    )
    .map(|statement| Program { statement })
    .parse_next(s)
}
