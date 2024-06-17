use winnow::{
    ascii::multispace0,
    combinator::{alt, cut_err, opt, preceded, repeat},
    seq,
    token::one_of,
    PResult, Parser,
};

use super::{
    literal::{parse_literal, Literal},
    path::{parse_path, Path},
    stmt::{parse_statment, Statement},
};

#[derive(Debug, Clone)]
pub enum Expression {
    Block(BlockExpression),
    Nonblock(NonblockExpression),
}

#[derive(Debug, Clone)]
pub enum NonblockExpression {
    Literal(Literal),
    Path(Path),
    Binary(BinaryOperation),
}

pub fn parse_nonblock_expression<'s>(input: &mut &'s str) -> PResult<NonblockExpression> {
    alt((
        parse_add_sub.map(NonblockExpression::Binary),
        parse_mul_div_mod.map(NonblockExpression::Binary),
        parse_path.map(NonblockExpression::Path),
        parse_literal.map(NonblockExpression::Literal),
    ))
    .parse_next(input)
}

pub fn parse_expression<'s>(input: &mut &'s str) -> PResult<Expression> {
    alt((
        parse_block_expression.map(Expression::Block),
        parse_nonblock_expression.map(Expression::Nonblock),
    ))
    .parse_next(input)
}

#[derive(Debug, Clone)]
pub enum BlockExpression {
    Block(Block),
}

pub fn parse_block_expression<'s>(input: &mut &'s str) -> PResult<BlockExpression> {
    parse_block.map(BlockExpression::Block).parse_next(input)
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statement: Vec<Statement>,
    pub result: Option<Box<Expression>>,
}

pub fn parse_block<'s>(input: &mut &'s str) -> PResult<Block> {
    seq!(
        _: '{',
        repeat(0.., preceded(multispace0, parse_statment)),
        opt(preceded(multispace0, parse_nonblock_expression)),
        _: multispace0,
        _: '}',
    )
    .map(|(statement, expression)| Block {
        statement,
        result: expression.map(Expression::Nonblock).map(Box::new),
    })
    .parse_next(input)
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub lhs: Box<Expression>,
    pub operator: char,
    pub rhs: Box<Expression>,
}

pub fn parse_add_sub<'s>(input: &mut &'s str) -> PResult<BinaryOperation> {
    seq!(
        alt((parse_mul_div_mod.map(NonblockExpression::Binary).map(Expression::Nonblock), parse_term)),
        _: multispace0,
        one_of(['+', '-']),
        _: multispace0,
        alt((
            parse_add_sub.map(NonblockExpression::Binary).map(Expression::Nonblock),
            parse_term,
        ))
    )
    .map(|(lhs, operator, rhs)| {
        BinaryOperation {
            lhs: Box::new(lhs),
            operator,
            rhs: Box::new(rhs),
        }
    })
    .parse_next(input)
}

pub fn parse_mul_div_mod<'s>(input: &mut &'s str) -> PResult<BinaryOperation> {
    seq!(
        parse_term,
        _: multispace0,
        one_of(['*', '/', '%']),
        _: multispace0,
        alt((
            parse_mul_div_mod.map(NonblockExpression::Binary).map(Expression::Nonblock),
            parse_term,
        ))
    )
    .map(|(lhs, operator, rhs)| BinaryOperation {
        lhs: Box::new(lhs),
        operator,
        rhs: Box::new(rhs),
    })
    .parse_next(input)
}

pub fn parse_term<'s>(input: &mut &'s str) -> PResult<Expression> {
    alt((
        parse_literal
            .map(NonblockExpression::Literal)
            .map(Expression::Nonblock),
        parse_path
            .map(NonblockExpression::Path)
            .map(Expression::Nonblock),
        parse_block_expression.map(Expression::Block),
    ))
    .parse_next(input)
}
