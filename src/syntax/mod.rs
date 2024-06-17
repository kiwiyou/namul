use stmt::{parse_statment, Statement};
use winnow::{
    ascii::multispace0,
    combinator::{preceded, repeat, terminated},
    PResult, Parser,
};

pub mod expr;
pub mod format;
pub mod item;
pub mod path;
pub mod stmt;

#[derive(Debug, Clone)]
pub struct Program {
    pub statement: Vec<Statement>,
}

pub fn parse_program<'s>(input: &mut &'s str) -> PResult<Program> {
    terminated(
        repeat(0.., preceded(multispace0, parse_statment)),
        multispace0,
    )
    .map(|statement| Program { statement })
    .parse_next(input)
}
