use winnow::{ascii::digit1, combinator::opt, token::one_of, PResult, Parser};

#[derive(Debug, Clone)]
pub enum Literal {
    Decimal(DecimalLiteral),
}

pub fn parse_literal<'s>(input: &mut &'s str) -> PResult<Literal> {
    parse_decimal_literal.map(Literal::Decimal).parse_next(input)
}

#[derive(Debug, Clone)]
pub struct DecimalLiteral(pub String);

pub fn parse_decimal_literal<'s>(input: &mut &'s str) -> PResult<DecimalLiteral> {
    (opt(one_of(['-', '+'])), digit1)
        .map(|(sign, digits)| {
            let mut literal = String::new();
            if let Some(sign) = sign {
                literal.push(sign);
            }
            literal.push_str(digits);
            DecimalLiteral(literal)
        })
        .parse_next(input)
}
