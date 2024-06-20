use statement::{parse_statement, Statement};
use winnow::{
    ascii::digit1,
    combinator::{opt, preceded, repeat, terminated},
    Located, PResult, Parser,
};

pub mod expression;
pub mod format;
pub mod item;
pub mod literal;
pub mod path;
pub mod statement;
pub mod token;

pub use token::{Token, TokenKind};

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

pub fn parse_usize(s: &mut Located<&str>) -> PResult<usize> {
    digit1
        .verify_map(|digits: &str| digits.parse().ok())
        .parse_next(s)
}
