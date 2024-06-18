use winnow::{
    combinator::{alt, delimited, opt, preceded, repeat},
    seq, Located, PResult, Parser,
};

use super::{
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
    If(If),
    Comparison(Comparison),
}

pub fn parse_nonblock_expression(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    alt((
        parse_if.map(NonblockExpression::If),
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
}

pub fn parse_block_expression(s: &mut Located<&str>) -> PResult<BlockExpression> {
    parse_block.map(BlockExpression::Block).parse_next(s)
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

pub fn parse_add_sub(s: &mut Located<&str>) -> PResult<BinaryOperation> {
    seq!(
        alt((parse_mul_div_mod.map(NonblockExpression::Binary).map(Expression::Nonblock), parse_term)),
        _: opt(TokenKind::White),
        alt((TokenKind::PunctPlusSign, TokenKind::PunctHyphenMinus)),
        _: opt(TokenKind::White),
        alt((
            parse_add_sub.map(NonblockExpression::Binary).map(Expression::Nonblock),
            parse_mul_div_mod.map(NonblockExpression::Binary).map(Expression::Nonblock),
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
    .parse_next(s)
}

pub fn parse_mul_div_mod(s: &mut Located<&str>) -> PResult<BinaryOperation> {
    seq!(
        parse_term,
        _: opt(TokenKind::White),
        alt((TokenKind::PunctAsterisk, TokenKind::PunctSolidus, TokenKind::PunctPercentSign)),
        _: opt(TokenKind::White),
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
    pub truthy: Box<Expression>,
    pub falsy: Option<Box<Expression>>,
}

pub fn parse_if(s: &mut Located<&str>) -> PResult<If> {
    seq!(
        _: TokenKind::KeywordIf,
        _: opt(TokenKind::White),
        parse_expression,
        _: opt(TokenKind::White),
        parse_expression,
        opt(
            preceded((opt(TokenKind::White), "else", opt(TokenKind::White)), parse_expression)
        )
    )
    .map(|(condition, truthy, falsy)| If {
        condition: Box::new(condition),
        truthy: Box::new(truthy),
        falsy: falsy.map(Box::new),
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
                    TokenKind::EqEq,
                    TokenKind::LtEq,
                    TokenKind::GtEq,
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
