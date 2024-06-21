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
    MakeArray(MakeArray),
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
        parse_print.map(NonblockExpression::Print),
        parse_select,
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
    .map(|(mut statement, expression): (Vec<_>, _)| {
        let result = if let Some(expression) = expression {
            Some(Box::new(Expression::Nonblock(expression)))
        } else {
            match statement.pop() {
                Some(Statement::Expression(Expression::Block(block))) => {
                    Some(Box::new(Expression::Block(block)))
                }
                Some(popped @ _) => {
                    statement.push(popped);
                    None
                }
                None => None,
            }
        };
        Block { statement, result }
    })
    .parse_next(s)
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub lhs: Box<Expression>,
    pub operator: Token,
    pub rhs: Box<Expression>,
}

pub fn parse_logical(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    let first = parse_comparison.parse_next(s)?;
    let Ok((op, rhs)) = seq!(
        _: opt(TokenKind::White),
        alt((TokenKind::PunctAmpersandAmpersand, TokenKind::PunctVerticalLineVerticalLine)),
        _: opt(TokenKind::White),
        parse_comparison,
    )
    .parse_next(s) else {
        return Ok(first);
    };
    let binary = NonblockExpression::Binary(BinaryOperation {
        lhs: Box::new(Expression::Nonblock(first)),
        operator: op,
        rhs: Box::new(Expression::Nonblock(rhs)),
    });
    repeat(
        0..,
        seq!(
            _: opt(TokenKind::White),
            alt((TokenKind::PunctAmpersandAmpersand, TokenKind::PunctVerticalLineVerticalLine)),
            _: opt(TokenKind::White),
            parse_comparison,
        ),
    )
    .fold(
        move || binary.clone(),
        |prev, (op, next)| {
            NonblockExpression::Binary(BinaryOperation {
                lhs: Box::new(Expression::Nonblock(prev)),
                operator: op,
                rhs: Box::new(Expression::Nonblock(next)),
            })
        },
    )
    .parse_next(s)
}

pub fn parse_add_sub(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    let first = parse_mul_div_mod.parse_next(s)?;
    let Ok((op, rhs)) = seq!(
        _: opt(TokenKind::White),
        alt((TokenKind::PunctPlusSign, TokenKind::PunctHyphenMinus)),
        _: opt(TokenKind::White),
        parse_mul_div_mod,
    )
    .parse_next(s) else {
        return Ok(first);
    };
    let binary = BinaryOperation {
        lhs: Box::new(Expression::Nonblock(first)),
        operator: op,
        rhs: Box::new(Expression::Nonblock(rhs)),
    };
    repeat(
        0..,
        seq!(
            _: opt(TokenKind::White),
            alt((TokenKind::PunctPlusSign, TokenKind::PunctHyphenMinus)),
            _: opt(TokenKind::White),
            parse_mul_div_mod,
        ),
    )
    .fold(
        move || binary.clone(),
        |prev, (op, next)| BinaryOperation {
            lhs: Box::new(Expression::Nonblock(NonblockExpression::Binary(prev))),
            operator: op,
            rhs: Box::new(Expression::Nonblock(next)),
        },
    )
    .map(|binary| NonblockExpression::Binary(binary))
    .parse_next(s)
}

pub fn parse_mul_div_mod(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    let first = parse_index.parse_next(s)?;

    let Ok((op, rhs)) = seq!(
        _: opt(TokenKind::White),
        alt((TokenKind::PunctAsterisk, TokenKind::PunctSolidus, TokenKind::PunctPercentSign)),
        _: opt(TokenKind::White),
        parse_index,
    )
    .parse_next(s) else {
        return Ok(first);
    };
    let binary = BinaryOperation {
        lhs: Box::new(Expression::Nonblock(first)),
        operator: op,
        rhs: Box::new(Expression::Nonblock(rhs)),
    };
    repeat(
        0..,
        seq!(
            _: opt(TokenKind::White),
            alt((TokenKind::PunctAsterisk, TokenKind::PunctSolidus, TokenKind::PunctPercentSign)),
            _: opt(TokenKind::White),
            parse_index
        ),
    )
    .fold(
        move || binary.clone(),
        |prev, (op, next)| BinaryOperation {
            lhs: Box::new(Expression::Nonblock(NonblockExpression::Binary(prev))),
            operator: op,
            rhs: Box::new(Expression::Nonblock(next)),
        },
    )
    .map(|binary| NonblockExpression::Binary(binary))
    .parse_next(s)
}

pub fn parse_term(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    alt((
        parse_parentheses,
        parse_make_tuple.map(NonblockExpression::MakeTuple),
        parse_make_array.map(NonblockExpression::MakeArray),
        parse_invocation.map(NonblockExpression::Invocation),
        parse_literal.map(NonblockExpression::Literal),
        parse_path.map(NonblockExpression::Path),
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

pub fn parse_comparison(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    let first = parse_add_sub.parse_next(s)?;
    let Ok(chain) = repeat(
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
            parse_add_sub.map(Expression::Nonblock),
        ),
    )
    .parse_next(s) else {
        return Ok(first);
    };
    Ok(NonblockExpression::Comparison(Comparison {
        first: Box::new(Expression::Nonblock(first)),
        chain,
    }))
}

#[derive(Debug, Clone)]
pub struct Select {
    pub condition: Box<Expression>,
    pub truthy: Box<Expression>,
    pub falsy: Box<Expression>,
}

pub fn parse_select(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    let first = parse_logical.parse_next(s)?;
    let Ok((truthy, falsy)) = seq!(
        _: opt(TokenKind::White),
        _: TokenKind::PunctQuestionMark,
        _: opt(TokenKind::White),
        parse_select,
        _: opt(TokenKind::White),
        _: TokenKind::PunctColon,
        _: opt(TokenKind::White),
        parse_select,
    )
    .parse_next(s) else {
        return Ok(first);
    };
    Ok(NonblockExpression::Select(Select {
        condition: Box::new(Expression::Nonblock(first)),
        truthy: Box::new(Expression::Nonblock(truthy)),
        falsy: Box::new(Expression::Nonblock(falsy)),
    }))
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
        parse_assignee_tuple.map(Assignee::Tuple),
        parse_index_only.map(Assignee::Index),
        parse_path.map(Assignee::Path),
        delimited(
            TokenKind::PunctLeftParenthesis,
            preceded(opt(TokenKind::White), parse_assignee),
            (opt(TokenKind::White), TokenKind::PunctRightParenthesis),
        ),
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
    alt((
        parse_index_only.map(Place::Index),
        parse_path.map(Place::Path),
    ))
    .parse_next(s)
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

pub fn parse_index(s: &mut Located<&str>) -> PResult<NonblockExpression> {
    let term = parse_term.parse_next(s)?;
    let Ok(other) = delimited(
        (
            opt(TokenKind::White),
            TokenKind::PunctLeftSquareBracket,
            opt(TokenKind::White),
        ),
        parse_expression,
        (opt(TokenKind::White), TokenKind::PunctRightSquareBracket),
    )
    .parse_next(s) else {
        return Ok(term);
    };
    let index = Index {
        target: Box::new(Expression::Nonblock(term)),
        index: Box::new(other),
    };
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
    .map(NonblockExpression::Index)
    .parse_next(s)
}

pub fn parse_index_only(s: &mut Located<&str>) -> PResult<Index> {
    let term = parse_term.parse_next(s)?;
    let other = delimited(
        (
            opt(TokenKind::White),
            TokenKind::PunctLeftSquareBracket,
            opt(TokenKind::White),
        ),
        parse_expression,
        (opt(TokenKind::White), TokenKind::PunctRightSquareBracket),
    )
    .parse_next(s)?;
    let index = Index {
        target: Box::new(Expression::Nonblock(term)),
        index: Box::new(other),
    };
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

#[derive(Debug, Clone)]
pub struct MakeArray {
    pub args: Vec<Expression>,
}

pub fn parse_make_array(s: &mut Located<&str>) -> PResult<MakeArray> {
    delimited(
        TokenKind::PunctLeftSquareBracket,
        separated(
            0..,
            preceded(opt(TokenKind::White), parse_expression),
            (opt(TokenKind::White), TokenKind::PunctComma),
        ),
        (
            opt(TokenKind::White),
            opt(TokenKind::PunctComma),
            opt(TokenKind::White),
            TokenKind::PunctRightSquareBracket,
        ),
    )
    .map(|args| MakeArray { args })
    .parse_next(s)
}
