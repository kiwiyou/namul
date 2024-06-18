use winnow::{
    combinator::{alt, delimited, opt, preceded, repeat},
    seq, Located, PResult, Parser,
};

use super::{
    format::{parse_format_string, FormatString},
    literal::{parse_literal, Literal},
    path::{parse_path, Path},
    stmt::{parse_statement, Statement},
    Token, TokenKind,
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
    Comparison(Comparison),
    Print(FormatString),
}

pub fn parse_nonblock_expression(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    alt((
        parse_format_string.map(NonblockExpression::Print),
        parse_logcial.map(NonblockExpression::Binary),
        parse_comparison.map(NonblockExpression::Comparison),
        parse_add_sub.map(NonblockExpression::Binary),
        parse_mul_div_mod.map(NonblockExpression::Binary),
        parse_literal.map(NonblockExpression::Literal),
        parse_path.map(NonblockExpression::Path),
    ))
    .parse_next(s)
}

pub fn parse_expression(s: &mut Located<&str>) -> PResult<Expression> {
    alt((
        delimited(
            (TokenKind::PunctLeftParenthesis, opt(TokenKind::White)),
            parse_expression,
            (opt(TokenKind::White), TokenKind::PunctRightParenthesis),
        ),
        parse_block_expression.map(Expression::Block),
        parse_nonblock_expression.map(Expression::Nonblock),
    ))
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub enum BlockExpression {
    Block(Block),
    If(If),
}

pub fn parse_block_expression(s: &mut Located<&str>) -> PResult<BlockExpression> {
    alt((
        parse_if.map(BlockExpression::If),
        parse_block.map(BlockExpression::Block),
    ))
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statement: Vec<Statement>,
    pub result: Option<Box<Expression>>,
}

pub fn parse_block(s: &mut Located<&str>) -> PResult<Block> {
    seq!(
        _: TokenKind::PunctLeftCurlyBracket,
        repeat(0.., preceded(opt(TokenKind::White), parse_statement)),
        opt(preceded(opt(TokenKind::White), parse_nonblock_expression)),
        _: opt(TokenKind::White),
        _: TokenKind::PunctRightCurlyBracket,
    )
    .map(|(statement, expression)| Block {
        statement,
        result: expression.map(Expression::Nonblock).map(Box::new),
    })
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub lhs: Box<Expression>,
    pub operator: Token,
    pub rhs: Box<Expression>,
}

pub fn parse_logcial(s: &mut Located<&str>) -> PResult<BinaryOperation> {
    let operand = || {
        alt((
            parse_comparison
                .map(NonblockExpression::Comparison)
                .map(Expression::Nonblock),
            parse_add_sub
                .map(NonblockExpression::Binary)
                .map(Expression::Nonblock),
            parse_mul_div_mod
                .map(NonblockExpression::Binary)
                .map(Expression::Nonblock),
            parse_term,
        ))
    };
    let first = seq!(
        operand(),
        _: opt(TokenKind::White),
        alt((TokenKind::PunctAmpersandAmpersand, TokenKind::PunctVerticalLineVerticalLine)),
        _: opt(TokenKind::White),
        operand(),
    )
    .map(|(lhs, op, rhs)| BinaryOperation {
        lhs: Box::new(lhs),
        operator: op,
        rhs: Box::new(rhs),
    })
    .parse_next(s)?;
    repeat(
        0..,
        seq!(
            _: opt(TokenKind::White),
        alt((TokenKind::PunctAmpersandAmpersand, TokenKind::PunctVerticalLineVerticalLine)),
        _: opt(TokenKind::White),
            operand(),
        ),
    )
    .fold(
        move || first.clone(),
        |prev, (op, next)| BinaryOperation {
            lhs: Box::new(Expression::Nonblock(NonblockExpression::Binary(prev))),
            operator: op,
            rhs: Box::new(next),
        },
    )
    .parse_next(s)
}

pub fn parse_add_sub(s: &mut Located<&str>) -> PResult<BinaryOperation> {
    let operand = || {
        alt((
            parse_mul_div_mod
                .map(NonblockExpression::Binary)
                .map(Expression::Nonblock),
            parse_term,
        ))
    };
    let first = seq!(
        operand(),
        _: opt(TokenKind::White),
        alt((TokenKind::PunctAsterisk, TokenKind::PunctSolidus, TokenKind::PunctPercentSign)),
        _: opt(TokenKind::White),
        operand(),
    )
    .map(|(lhs, op, rhs)| BinaryOperation {
        lhs: Box::new(lhs),
        operator: op,
        rhs: Box::new(rhs),
    })
    .parse_next(s)?;
    repeat(
        0..,
        seq!(
            _: opt(TokenKind::White),
            alt((TokenKind::PunctAsterisk, TokenKind::PunctSolidus, TokenKind::PunctPercentSign)),
            _: opt(TokenKind::White),
            operand(),
        ),
    )
    .fold(
        move || first.clone(),
        |prev, (op, next)| BinaryOperation {
            lhs: Box::new(Expression::Nonblock(NonblockExpression::Binary(prev))),
            operator: op,
            rhs: Box::new(next),
        },
    )
    .parse_next(s)
}

pub fn parse_mul_div_mod(s: &mut Located<&str>) -> PResult<BinaryOperation> {
    let first = seq!(
        parse_term,
        _: opt(TokenKind::White),
        alt((TokenKind::PunctAsterisk, TokenKind::PunctSolidus, TokenKind::PunctPercentSign)),
        _: opt(TokenKind::White),
        parse_term,
    )
    .map(|(lhs, op, rhs)| BinaryOperation {
        lhs: Box::new(lhs),
        operator: op,
        rhs: Box::new(rhs),
    })
    .parse_next(s)?;
    repeat(
        0..,
        seq!(
            _: opt(TokenKind::White),
            alt((TokenKind::PunctAsterisk, TokenKind::PunctSolidus, TokenKind::PunctPercentSign)),
            _: opt(TokenKind::White),
            parse_term,
        ),
    )
    .fold(
        move || first.clone(),
        |prev, (op, next)| BinaryOperation {
            lhs: Box::new(Expression::Nonblock(NonblockExpression::Binary(prev))),
            operator: op,
            rhs: Box::new(next),
        },
    )
    .parse_next(s)
}

pub fn parse_term(s: &mut Located<&str>) -> PResult<Expression> {
    alt((
        parse_literal
            .map(NonblockExpression::Literal)
            .map(Expression::Nonblock),
        parse_path
            .map(NonblockExpression::Path)
            .map(Expression::Nonblock),
        parse_block_expression.map(Expression::Block),
    ))
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Box<Expression>,
    pub truthy: Block,
    pub falsy: Option<Block>,
}

pub fn parse_if(s: &mut Located<&str>) -> PResult<If> {
    seq!(
        _: TokenKind::KeywordIf,
        _: opt(TokenKind::White),
        parse_expression,
        _: opt(TokenKind::White),
        parse_block,
        opt(
            preceded((opt(TokenKind::White), "else", opt(TokenKind::White)), parse_block)
        )
    )
    .map(|(condition, truthy, falsy)| If {
        condition: Box::new(condition),
        truthy,
        falsy,
    })
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct Comparison {
    pub first: Box<Expression>,
    pub chain: Vec<(Token, Expression)>,
}

pub fn parse_comparison(s: &mut Located<&str>) -> PResult<Comparison> {
    (
        alt((
            parse_add_sub
                .map(NonblockExpression::Binary)
                .map(Expression::Nonblock),
            parse_mul_div_mod
                .map(NonblockExpression::Binary)
                .map(Expression::Nonblock),
            parse_term,
        )),
        repeat(
            1..,
            seq!(
                _: opt(TokenKind::White),
                alt((
                    TokenKind::PunctLessThanSign,
                    TokenKind::PunctGreaterThanSign,
                    TokenKind::PunctEqualsSignEqualsSign,
                    TokenKind::PunctExclamationMarkEqualsSign,
                    TokenKind::PunctLessThanSignEqualsSign,
                    TokenKind::PunctGreaterThanSignEqualsSign,
                )),
                _: opt(TokenKind::White),
                alt((
                    parse_add_sub
                        .map(NonblockExpression::Binary)
                        .map(Expression::Nonblock),
                    parse_mul_div_mod
                        .map(NonblockExpression::Binary)
                        .map(Expression::Nonblock),
                    parse_term
                ))
            ),
        ),
    )
        .map(|(first, chain)| Comparison {
            first: Box::new(first),
            chain,
        })
        .parse_next(s)
}
