use winnow::{
    combinator::{alt, delimited, opt, preceded, separated, separated_pair, terminated},
    seq, Located, PResult, Parser,
};

use crate::syntax::expression::parse_expression;

use super::{
    expression::{
        parse_assignee, parse_block, parse_block_expression, parse_nonblock_expression, Assignee,
        Block, Expression,
    },
    item::{parse_type, Type},
    Token, TokenKind,
};

#[derive(Debug, Clone)]
pub enum Statement {
    Nop,
    Input(InputParser),
    Expression(Expression),
    Repeat(Repeat),
    While(While),
    Function(Function),
}

pub fn parse_statement(s: &mut Located<&str>) -> PResult<Statement> {
    alt((
        TokenKind::PunctSemicolon.value(Statement::Nop),
        parse_function.map(Statement::Function),
        parse_input_parser.map(Statement::Input),
        parse_repeat.map(Statement::Repeat),
        parse_while.map(Statement::While),
        parse_block_expression
            .map(Expression::Block)
            .map(Statement::Expression),
        terminated(
            parse_nonblock_expression,
            (opt(TokenKind::White), TokenKind::PunctSemicolon),
        )
        .map(Expression::Nonblock)
        .map(Statement::Expression),
    ))
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
        parse_declaration.map(Pattern::Declaration),
        TokenKind::Identifier.map(Pattern::Ident),
        parse_tuple_pattern,
    ))
    .parse_next(s)
}

pub fn parse_tuple_pattern(s: &mut Located<&str>) -> PResult<Pattern> {
    delimited(
        TokenKind::PunctLeftParenthesis,
        separated(
            0..,
            preceded(opt(TokenKind::White), parse_pattern),
            (opt(TokenKind::White), TokenKind::PunctComma),
        ),
        (
            opt(TokenKind::White),
            opt(TokenKind::PunctComma),
            opt(TokenKind::White),
            TokenKind::PunctRightParenthesis,
        ),
    )
    .map(Pattern::Tuple)
    .parse_next(s)
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
    pub arg: Vec<Assignee>,
}

pub fn parse_input_parser(s: &mut Located<&str>) -> PResult<InputParser> {
    delimited(
        TokenKind::PunctVerticalLine,
        separated(
            1..,
            preceded(opt(TokenKind::White), parse_assignee),
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

#[derive(Debug, Clone)]
pub struct Function {
    pub ident: Token,
    pub args: Vec<Pattern>,
    pub result: Option<Type>,
    pub block: Block,
}

fn is_pattern_typed(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Ident(_) => false,
        Pattern::Declaration(_) => true,
        Pattern::Tuple(args) => args.iter().all(is_pattern_typed),
    }
}

pub fn parse_function(s: &mut Located<&str>) -> PResult<Function> {
    seq!(
        _: TokenKind::KeywordFn,
        _: opt(TokenKind::White),
        TokenKind::Identifier,
        _: opt(TokenKind::White),
        parse_pattern,
        _: opt(TokenKind::White),
        opt(parse_type),
        _: opt(TokenKind::White),
        parse_block,
    )
    .verify(|(_, args, _, _)| is_pattern_typed(args))
    .verify_map(|(ident, args, result, block)| {
        if let Pattern::Tuple(args) = args {
            Some(Function {
                ident,
                args,
                result,
                block,
            })
        } else {
            None
        }
    })
    .parse_next(s)
}
