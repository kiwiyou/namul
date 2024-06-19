use winnow::{
    combinator::{alt, delimited, opt, preceded, separated, separated_pair, terminated},
    seq, Located, PResult, Parser,
};

use crate::syntax::expr::parse_expression;

use super::{
    expr::{parse_block, parse_block_expression, parse_nonblock_expression, Block, Expression},
    item::{parse_type, Type},
    Token, TokenKind,
};

#[derive(Debug, Clone)]
pub enum Statement {
    Nop,
    Input(InputParser),
    Expression(Expression),
    Assignment(Assignment),
    Repeat(Repeat),
    While(While),
}

pub fn parse_statement(s: &mut Located<&str>) -> PResult<Statement> {
    alt((
        TokenKind::PunctSemicolon.value(Statement::Nop),
        parse_input_parser.map(Statement::Input),
        parse_repeat.map(Statement::Repeat),
        parse_while.map(Statement::While),
        terminated(
            parse_nonblock_expression,
            (opt(TokenKind::White), TokenKind::PunctSemicolon),
        )
        .map(|nonblock| Statement::Expression(Expression::Nonblock(nonblock))),
        parse_block_expression.map(|block| Statement::Expression(Expression::Block(block))),
        terminated(
            parse_assignment,
            (opt(TokenKind::White), TokenKind::PunctSemicolon),
        )
        .map(Statement::Assignment),
    ))
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub lhs: Pattern,
    pub rhs: Expression,
}

pub fn parse_assignment(s: &mut Located<&str>) -> PResult<Assignment> {
    seq!(
        parse_pattern,
        _: opt(TokenKind::White),
        _: TokenKind::PunctEqualsSign,
        _: opt(TokenKind::White),
        parse_expression,
    )
    .map(|(lhs, rhs)| Assignment { lhs, rhs })
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(Token),
    Declaration(Declaration),
    Tuple(Vec<Pattern>),
}

pub fn parse_pattern(s: &mut Located<&str>) -> PResult<Pattern> {
    alt((
        parse_tuple_pattern,
        parse_declaration.map(Pattern::Declaration),
        TokenKind::Identifier.map(Pattern::Ident),
    ))
    .parse_next(s)
}

pub fn parse_tuple_pattern(s: &mut Located<&str>) -> PResult<Pattern> {
    seq!(
        _: TokenKind::PunctLeftParenthesis,
        _: opt(TokenKind::White),
        separated(2.., parse_pattern, (opt(TokenKind::White), TokenKind::PunctComma, opt(TokenKind::White))),
        _: opt(TokenKind::White),
        _: opt(TokenKind::PunctComma),
        _: opt(TokenKind::White),
        _: TokenKind::PunctRightParenthesis,
    ).map(|(args, )| Pattern::Tuple(args)).parse_next(s)
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub ty: Type,
    pub ident: Token,
}

pub fn parse_declaration(s: &mut Located<&str>) -> PResult<Declaration> {
    separated_pair(parse_type, TokenKind::White, TokenKind::Identifier)
        .map(|(ty, ident)| Declaration { ty, ident })
        .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct Repeat {
    pub times: Expression,
    pub block: Block,
}

pub fn parse_repeat(s: &mut Located<&str>) -> PResult<Repeat> {
    seq!(
        _: TokenKind::KeywordRep,
        _: opt(TokenKind::White),
        parse_expression,
        _: opt(TokenKind::White),
        parse_block,
    )
    .map(|(times, expr)| Repeat { times, block: expr })
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct InputParser {
    pub arg: Vec<(Token, Token)>,
}

pub fn parse_input_parser(s: &mut Located<&str>) -> PResult<InputParser> {
    delimited(
        TokenKind::PunctVerticalLine,
        separated(
            1..,
            preceded(opt(TokenKind::White), parse_input_arg),
            TokenKind::PunctComma,
        ),
        (
            opt((opt(TokenKind::White), TokenKind::PunctComma)),
            opt(TokenKind::White),
            TokenKind::PunctVerticalLine,
        ),
    )
    .map(|arg| InputParser { arg })
    .parse_next(s)
}

pub fn parse_input_arg(s: &mut Located<&str>) -> PResult<(Token, Token)> {
    separated_pair(
        TokenKind::Identifier,
        TokenKind::White,
        TokenKind::Identifier,
    )
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expression,
    pub block: Block,
}

pub fn parse_while(s: &mut Located<&str>) -> PResult<While> {
    seq!(
        _: TokenKind::KeywordWhile,
        _: opt(TokenKind::White),
        parse_expression,
        _: opt(TokenKind::White),
        parse_block,
    )
    .map(|(condition, block)| While { condition, block })
    .parse_next(s)
}
