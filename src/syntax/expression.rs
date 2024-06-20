use winnow::{
    combinator::{alt, delimited, opt, preceded, repeat, separated, separated_pair, terminated},
    seq, Located, PResult, Parser,
};

use super::{
    format::{parse_format_string, FormatString},
    item::{parse_type, Type},
    literal::{parse_literal, Literal},
    path::{parse_path, Path},
    statement::{parse_statement, Statement},
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
    Print(Print),
    Select(Select),
    MakeTuple(MakeTuple),
    Invocation(Invocation),
    Assignment(Assignment),
    Declaration(Declaration),
    CompoundAssignment(CompoundAssignment),
    Index(Index),
}

pub fn parse_nonblock_expression(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    alt((
        parse_assignment.map(NonblockExpression::Assignment),
        parse_compound_assignment.map(NonblockExpression::CompoundAssignment),
        parse_declaration.map(NonblockExpression::Declaration),
        parse_invocation.map(NonblockExpression::Invocation),
        parse_select.map(NonblockExpression::Select),
        parse_logical.map(NonblockExpression::Binary),
        parse_comparison.map(NonblockExpression::Comparison),
        parse_add_sub.map(NonblockExpression::Binary),
        parse_mul_div_mod.map(NonblockExpression::Binary),
        parse_print.map(NonblockExpression::Print),
        parse_index.map(NonblockExpression::Index),
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
        _: opt(TokenKind::White),
        opt(parse_nonblock_expression),
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
            parse_index
                .map(NonblockExpression::Index)
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
            parse_index
                .map(NonblockExpression::Index)
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
    let operand = || {
        alt((
            parse_index
                .map(NonblockExpression::Index)
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

pub fn parse_term(s: &mut Located<&str>) -> PResult<Expression> {
    alt((
        parse_block_expression.map(Expression::Block),
        parse_invocation
            .map(NonblockExpression::Invocation)
            .map(Expression::Nonblock),
        parse_parentheses.map(Expression::Nonblock),
        parse_make_tuple
            .map(NonblockExpression::MakeTuple)
            .map(Expression::Nonblock),
        parse_literal
            .map(NonblockExpression::Literal)
            .map(Expression::Nonblock),
        parse_path
            .map(NonblockExpression::Path)
            .map(Expression::Nonblock),
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
    let operand = || {
        alt((
            parse_add_sub
                .map(NonblockExpression::Binary)
                .map(Expression::Nonblock),
            parse_mul_div_mod
                .map(NonblockExpression::Binary)
                .map(Expression::Nonblock),
            parse_index
                .map(NonblockExpression::Index)
                .map(Expression::Nonblock),
            parse_term,
        ))
    };
    (
        operand(),
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
                operand()
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
            parse_index
                .map(NonblockExpression::Index)
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
                parse_index
                .map(NonblockExpression::Index)
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

#[derive(Debug, Clone)]
pub enum Assignee {
    Declaration(Declaration),
    Path(Path),
    Tuple(Vec<Assignee>),
    Index(Index),
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub ty: Type,
    pub ident: Token,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub lhs: Assignee,
    pub rhs: Box<Expression>,
}

pub fn parse_assignment(s: &mut Located<&str>) -> PResult<Assignment> {
    separated_pair(
        parse_assignee,
        (
            opt(TokenKind::White),
            TokenKind::PunctEqualsSign,
            opt(TokenKind::White),
        ),
        parse_expression,
    )
    .map(|(lhs, rhs)| Assignment {
        lhs,
        rhs: Box::new(rhs),
    })
    .parse_next(s)
}

pub fn parse_assignee(s: &mut Located<&str>) -> PResult<Assignee> {
    alt((
        parse_declaration.map(Assignee::Declaration),
        parse_index.map(Assignee::Index),
        parse_path.map(Assignee::Path),
        delimited(
            TokenKind::PunctLeftParenthesis,
            preceded(opt(TokenKind::White), parse_assignee),
            (opt(TokenKind::White), TokenKind::PunctRightParenthesis),
        ),
        parse_assignee_tuple.map(Assignee::Tuple),
    ))
    .parse_next(s)
}

pub fn parse_declaration(s: &mut Located<&str>) -> PResult<Declaration> {
    separated_pair(parse_type, TokenKind::White, TokenKind::Identifier)
        .map(|(ty, ident)| Declaration { ty, ident })
        .parse_next(s)
}

pub fn parse_assignee_tuple(s: &mut Located<&str>) -> PResult<Vec<Assignee>> {
    delimited(
        TokenKind::PunctLeftParenthesis,
        separated(
            0..,
            preceded(opt(TokenKind::White), parse_assignee),
            (opt(TokenKind::White), TokenKind::PunctComma),
        ),
        (
            opt(TokenKind::White),
            opt(TokenKind::PunctComma),
            opt(TokenKind::White),
            TokenKind::PunctRightParenthesis,
        ),
    )
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub enum Place {
    Path(Path),
    Index(Index),
}

#[derive(Debug, Clone)]
pub struct CompoundAssignment {
    pub lhs: Place,
    pub op: Token,
    pub rhs: Box<Expression>,
}

pub fn parse_place(s: &mut Located<&str>) -> PResult<Place> {
    alt((parse_index.map(Place::Index), parse_path.map(Place::Path))).parse_next(s)
}

pub fn parse_compound_assignment(s: &mut Located<&str>) -> PResult<CompoundAssignment> {
    seq!(
        parse_place,
        _: opt(TokenKind::White),
        alt((
            TokenKind::PunctPlusSignEqualsSign,
            TokenKind::PunctHyphenMinusEqualsSign,
            TokenKind::PunctAsteriskEqualsSign,
            TokenKind::PunctSolidusEqualsSign,
            TokenKind::PunctPercentSignEqualsSign,
        )),
        _: opt(TokenKind::White),
        parse_expression,
    )
    .map(|(lhs, op, rhs)| CompoundAssignment {
        lhs,
        op,
        rhs: Box::new(rhs),
    })
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct Print {
    pub format: FormatString,
    pub args: Vec<Expression>,
}

pub fn parse_print(s: &mut Located<&str>) -> PResult<Print> {
    terminated(
        (
            parse_format_string,
            repeat(
                0..,
                preceded(
                    (
                        opt(TokenKind::White),
                        TokenKind::PunctComma,
                        opt(TokenKind::White),
                    ),
                    parse_expression,
                ),
            ),
        ),
        (opt(TokenKind::White), opt(TokenKind::PunctComma)),
    )
    .map(|(format, args)| Print { format, args })
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct Index {
    pub target: Box<Expression>,
    pub index: Box<Expression>,
}

pub fn parse_index(s: &mut Located<&str>) -> PResult<Index> {
    let index = (
        parse_term,
        delimited(
            (
                opt(TokenKind::White),
                TokenKind::PunctLeftSquareBracket,
                opt(TokenKind::White),
            ),
            parse_expression,
            (opt(TokenKind::White), TokenKind::PunctRightSquareBracket),
        ),
    )
        .map(|(target, index)| Index {
            target: Box::new(target),
            index: Box::new(index),
        })
        .parse_next(s)?;
    repeat(
        0..,
        delimited(
            (
                opt(TokenKind::White),
                TokenKind::PunctLeftSquareBracket,
                opt(TokenKind::White),
            ),
            parse_expression,
            (opt(TokenKind::White), TokenKind::PunctRightSquareBracket),
        ),
    )
    .fold(
        move || index.clone(),
        |prev, index| Index {
            target: Box::new(Expression::Nonblock(NonblockExpression::Index(prev))),
            index: Box::new(index),
        },
    )
    .parse_next(s)
}
