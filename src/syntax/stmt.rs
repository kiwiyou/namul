use winnow::{
    ascii::{multispace0, multispace1},
    combinator::{alt, delimited, opt, preceded, separated, separated_pair, terminated},
    seq, PResult, Parser,
};

use crate::syntax::expr::parse_expression;

use super::{
    expr::{parse_block, parse_block_expression, parse_nonblock_expression, Block, Expression},
    format::{parse_format_string, FormatString},
    item::{parse_type, Type},
    path::{parse_ident, Identifier},
};

#[derive(Debug, Clone)]
pub enum Statement {
    Nop,
    Input(InputParser),
    Expression(Expression),
    Print(FormatString),
    Assignment(Assignment),
    Repeat(Repeat),
}

pub fn parse_statment<'s>(input: &mut &'s str) -> PResult<Statement> {
    alt((
        ';'.value(Statement::Nop),
        terminated(parse_format_string, (multispace0, ';')).map(Statement::Print),
        terminated(parse_input_parser, (multispace0, ';')).map(Statement::Input),
        parse_repeat.map(Statement::Repeat),
        terminated(parse_nonblock_expression, (multispace0, ';'))
            .map(|nonblock| Statement::Expression(Expression::Nonblock(nonblock))),
        parse_block_expression.map(|block| Statement::Expression(Expression::Block(block))),
        terminated(parse_assignment, (multispace0, ';')).map(Statement::Assignment),
    ))
    .parse_next(input)
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub lhs: Pattern,
    pub rhs: Expression,
}

pub fn parse_assignment<'s>(input: &mut &'s str) -> PResult<Assignment> {
    seq!(
        parse_pattern,
        _: multispace0,
        _: '=',
        _: multispace0,
        parse_expression,
    )
    .map(|(lhs, rhs)| Assignment { lhs, rhs })
    .parse_next(input)
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(Identifier),
    Declaration(Declaration),
}

pub fn parse_pattern<'s>(input: &mut &'s str) -> PResult<Pattern> {
    alt((
        parse_declaration.map(Pattern::Declaration),
        parse_ident.map(Pattern::Ident),
    ))
    .parse_next(input)
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub ty: Type,
    pub ident: Identifier,
}

pub fn parse_declaration<'s>(input: &mut &'s str) -> PResult<Declaration> {
    separated_pair(parse_type, multispace1, parse_ident)
        .map(|(ty, ident)| Declaration { ty, ident })
        .parse_next(input)
}

#[derive(Debug, Clone)]
pub struct Repeat {
    pub times: Expression,
    pub block: Block,
}

pub fn parse_repeat<'s>(input: &mut &'s str) -> PResult<Repeat> {
    seq!(
        _: "rep",
        _: multispace1,
        parse_expression,
        _: multispace1,
        alt((
            parse_block,
            parse_input_parser.map(|parser| Block { statement: vec![Statement::Input(parser)], result: None })
        )),
    )
    .map(|(times, expr)| Repeat {
        times,
        block: expr,
    })
    .parse_next(input)
}

#[derive(Debug, Clone)]
pub struct InputParser {
    pub arg: Vec<(Type, Identifier)>,
}

pub fn parse_input_parser<'s>(input: &mut &'s str) -> PResult<InputParser> {
    delimited(
        '|',
        separated(1.., preceded(multispace0, parse_input_arg), ','),
        (opt((multispace0, ',')), multispace0, '|'),
    )
    .map(|arg| InputParser { arg })
    .parse_next(input)
}

pub fn parse_input_arg<'s>(input: &mut &'s str) -> PResult<(Type, Identifier)> {
    separated_pair(parse_type, multispace1, parse_ident).parse_next(input)
}
