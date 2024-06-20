use winnow::{
    combinator::{alt, delimited, opt, preceded, repeat, separated},
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
    Parentheses(Box<Expression>),
    Literal(Literal),
    Path(Path),
    Binary(BinaryOperation),
    Comparison(Comparison),
    Print(FormatString),
    Select(Select),
    MakeTuple(MakeTuple),
    Invocation(Invocation),
}

pub fn parse_nonblock_expression(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    alt((
        parse_invocation.map(NonblockExpression::Invocation),
        parse_select.map(NonblockExpression::Select),
        parse_logical.map(NonblockExpression::Binary),
        parse_comparison.map(NonblockExpression::Comparison),
        parse_add_sub.map(NonblockExpression::Binary),
        parse_mul_div_mod.map(NonblockExpression::Binary),
        parse_format_string.map(NonblockExpression::Print),
        parse_literal.map(NonblockExpression::Literal),
        parse_path.map(NonblockExpression::Path),
        parse_parentheses,
        parse_make_tuple.map(NonblockExpression::MakeTuple),
    ))
    .parse_next(s)
}

pub fn parse_expression(s: &mut Located<&str>) -> PResult<Expression> {
    alt((
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

pub fn parse_logical(s: &mut Located<&str>) -> PResult<BinaryOperation> {
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
        alt((TokenKind::PunctPlusSign, TokenKind::PunctHyphenMinus)),
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
            alt((TokenKind::PunctPlusSign, TokenKind::PunctHyphenMinus)),
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
        parse_invocation
            .map(NonblockExpression::Invocation)
            .map(Expression::Nonblock),
        parse_parentheses.map(Expression::Nonblock),
        parse_make_tuple
            .map(NonblockExpression::MakeTuple)
            .map(Expression::Nonblock),
        parse_format_string
            .map(NonblockExpression::Print)
            .map(Expression::Nonblock),
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

#[derive(Debug, Clone)]
pub struct Select {
    pub condition: Box<Expression>,
    pub truthy: Box<Expression>,
    pub falsy: Box<Expression>,
}

pub fn parse_select(s: &mut Located<&str>) -> PResult<Select> {
    let operand = || {
        alt((
            parse_select
                .map(NonblockExpression::Select)
                .map(Expression::Nonblock),
            parse_logical
                .map(NonblockExpression::Binary)
                .map(Expression::Nonblock),
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
    seq!(
        alt((
            parse_logical
                .map(NonblockExpression::Binary)
                .map(Expression::Nonblock),
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
        )),
        _: opt(TokenKind::White),
        _: TokenKind::PunctQuestionMark,
        _: opt(TokenKind::White),
        operand(),
        _: opt(TokenKind::White),
        _: TokenKind::PunctColon,
        _: opt(TokenKind::White),
        operand(),
    )
    .map(|(condition, truthy, falsy)| Select {
        condition: Box::new(condition),
        truthy: Box::new(truthy),
        falsy: Box::new(falsy),
    })
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct MakeTuple {
    pub args: Vec<Expression>,
}

pub fn parse_make_tuple(s: &mut Located<&str>) -> PResult<MakeTuple> {
    delimited(
        TokenKind::PunctLeftParenthesis,
        separated(
            0..,
            preceded(opt(TokenKind::White), parse_expression),
            (opt(TokenKind::White), TokenKind::PunctComma),
        ),
        (
            opt(TokenKind::White),
            opt(TokenKind::PunctComma),
            opt(TokenKind::White),
            TokenKind::PunctRightParenthesis,
        ),
    )
    .map(|args| MakeTuple { args })
    .parse_next(s)
}

pub fn parse_parentheses(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    delimited(
        (TokenKind::PunctLeftParenthesis, opt(TokenKind::White)),
        parse_expression,
        (opt(TokenKind::White), TokenKind::PunctRightParenthesis),
    )
    .map(|expr| NonblockExpression::Parentheses(Box::new(expr)))
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct Invocation {
    pub callee: Box<Expression>,
    pub args: Vec<Expression>,
}

pub fn parse_invocation(s: &mut Located<&str>) -> PResult<Invocation> {
    seq!(
        alt((
            parse_path.map(NonblockExpression::Path).map(Expression::Nonblock),
            parse_parentheses.map(Expression::Nonblock),
        )),
        _: opt(TokenKind::White),
        _: TokenKind::PunctLeftParenthesis,
        separated(0.., preceded(opt(TokenKind::White), parse_expression), (opt(TokenKind::White), TokenKind::PunctComma)),
        _: opt(TokenKind::White),
        _: opt(TokenKind::PunctComma),
        _: opt(TokenKind::White),
        _: TokenKind::PunctRightParenthesis,
    ).map(|(callee, args)|Invocation { callee: Box::new(callee), args }).parse_next(s)
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Box<Expression>,
}

pub fn parse_return(s: &mut Located<&str>) -> PResult<Return> {
    preceded(
        (TokenKind::KeywordReturn, opt(TokenKind::White)),
        parse_expression,
    )
    .map(|value| Return {
        value: Box::new(value),
    })
    .parse_next(s)
}
