use winnow::{Located, PResult, Parser};

use super::{Token, TokenKind};

#[derive(Debug, Clone)]
pub enum Path {
    Simple(Token),
}

pub fn parse_path(s: &mut Located<&str>) -> PResult<Path> {
    TokenKind::Identifier.map(Path::Simple).parse_next(s)
}
