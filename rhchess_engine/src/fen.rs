use crate::board;
use std::rc::Rc;
#[derive(Debug, PartialEq, Eq)]
enum TokenKind {
    Piece,
    Slash,
    Number,
    Whitespace,
    Color,
    Dash,
    Square,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Piece => "piece",
                TokenKind::Slash => "slash",
                TokenKind::Number => "number",
                TokenKind::Whitespace => "whitespace",
                TokenKind::Color => "color",
                TokenKind::Dash => "dash",
                TokenKind::Square => "square",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    lexme: String,
    idx: usize,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}: {}, {}", self.kind, &self.lexme, self.idx)
    }
}

struct Lexer {
    string: Vec<char>,
    cursor: usize,
}

impl Lexer {
    fn piece(&mut self) -> Option<Token> {
        use TokenKind::*;
        let cursor = self.cursor;
        let &head = self.string.get(cursor)?;
        if "prnbqk".contains(head.to_ascii_lowercase()) {
            self.cursor += 1;
            Some(Token {
                kind: Piece,
                lexme: head.to_string(),
                idx: cursor,
            })
        } else {
            None
        }
    }
    fn slash(&mut self) -> Option<Token> {
        use TokenKind::*;
        let cursor = self.cursor;
        match self.string.get(cursor) {
            Some('/') => {
                self.cursor += 1;
                Some(Token {
                    kind: Slash,
                    lexme: '/'.to_string(),
                    idx: cursor,
                })
            }
            _ => None,
        }
    }
    fn number(&mut self) -> Option<Token> {
        use TokenKind::*;
        let cursor = self.cursor;
        let mut lexme = String::new();
        for &i in self.string.get(cursor..)? {
            if !i.is_ascii_digit() {
                break;
            }
            self.cursor += 1;
            lexme.push(i);
        }
        if cursor == self.cursor {
            None
        } else {
            Some(Token {
                kind: Number,
                lexme,
                idx: cursor,
            })
        }
    }
    fn whitespace(&mut self) -> Option<Token> {
        use TokenKind::*;
        let cursor = self.cursor;
        let mut lexme = String::new();
        for &i in self.string.get(cursor..)? {
            if !i.is_whitespace() {
                break;
            }
            self.cursor += 1;
            lexme.push(i);
        }
        if cursor == self.cursor {
            None
        } else {
            Some(Token {
                kind: Whitespace,
                lexme,
                idx: cursor,
            })
        }
    }
    fn color(&mut self) -> Option<Token> {
        use TokenKind::*;
        let cursor = self.cursor;
        match self.string.get(cursor)? {
            c @ 'w' | c @ 'b' => {
                self.cursor += 1;
                Some(Token {
                    kind: Color,
                    lexme: c.to_string(),
                    idx: cursor,
                })
            }
            _ => None,
        }
    }
    fn dash(&mut self) -> Option<Token> {
        use TokenKind::*;
        let cursor = self.cursor;
        match self.string.get(cursor)? {
            '-' => {
                self.cursor += 1;
                Some(Token {
                    kind: Dash,
                    lexme: "-".to_string(),
                    idx: cursor,
                })
            }
            _ => None,
        }
    }
    fn square(&mut self) -> Option<Token> {
        use TokenKind::*;
        let cursor = self.cursor;
        let square = self.string.get(cursor..cursor + 2)?;
        let &rank = square.get(0)?;
        let &file = square.get(1)?;
        if rank >= 'a' && rank <= 'h' {
            if let Some(_) = file.to_digit(10) {
                self.cursor += 2;
                return Some(Token {
                    kind: Square,
                    lexme: square.iter().collect(),
                    idx: cursor,
                });
            }
        }
        return None;
    }
    fn new(s: &str) -> Self {
        Self {
            string: s.chars().collect(),
            cursor: 0,
        }
    }
    fn piece_or_color(&mut self) -> Option<Token> {
        if self.cursor > 0 && self.string.get(self.cursor - 1)?.is_whitespace() {
            self.color().or_else(|| self.piece())
        } else {
            self.piece().or_else(|| self.color())
        }
    }
    fn run(&mut self) -> Option<Vec<Rc<Token>>> {
        let mut ret = Vec::new();
        while self.cursor < self.string.len() {
            let tok = self
                .slash()
                .or_else(|| self.number())
                .or_else(|| self.square())
                .or_else(|| self.dash())
                .or_else(|| self.piece_or_color())
                .or_else(|| self.whitespace())?;
            ret.push(Rc::new(tok));
        }
        Some(ret)
    }
}

#[derive(Debug)]
pub struct Expected(Vec<TokenKind>);

impl Expected {
    fn append(mut self, Expected(mut v): Expected) -> Self {
        self.0.append(&mut v);
        self
    }
}

impl std::fmt::Display for Expected {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        for (idx, exp) in self.0.iter().enumerate() {
            if idx != 0 {
                write!(f, " | ")?;
            }
            write!(f, "{}", exp)?;
        }
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("unexpected end of file")]
    UnexpectedEOF,
    #[error("expected end of file")]
    ExpectedEOF,
    #[error("unexpected token {0}, expected: {1}")]
    Unexpected(Rc<Token>, Expected),
    #[error("the halfmove number is too large: {1}, on {0}")]
    TooLargeHalfMove(usize, u32),
    #[error("the file {0} is too large")]
    FileOutOfBounds(u32),
    #[error("The token {0} does not correspond to a castling piece")]
    InvalidCastlingSide(Rc<Token>),
}

struct Parser {
    string: Vec<Rc<Token>>,
    cursor: usize,
}

impl Parser {
    fn piece(&mut self) -> Result<board::Piece, ParserError> {
        let token = self
            .string
            .get(self.cursor)
            .ok_or(ParserError::UnexpectedEOF)?;
        match token.kind {
            TokenKind::Piece => {
                let p = &token.lexme.chars().next().unwrap();
                let owner = if p.is_ascii_lowercase() {
                    board::Player::Black
                } else {
                    board::Player::White
                };
                let kind = match p.to_ascii_lowercase() {
                    'p' => board::PieceKind::Pawn,
                    'r' => board::PieceKind::Rook,
                    'n' => board::PieceKind::Knight,
                    'b' => board::PieceKind::Bishop,
                    'q' => board::PieceKind::Queen,
                    'k' => board::PieceKind::King,
                    _ => unreachable!(),
                };
                self.cursor += 1;
                Ok(board::Piece { owner, kind })
            }
            _ => Err(ParserError::Unexpected(
                token.clone(),
                Expected(vec![TokenKind::Piece]),
            )),
        }
    }
    fn dash(&mut self) -> Result<(), ParserError> {
        let token = self
            .string
            .get(self.cursor)
            .ok_or(ParserError::UnexpectedEOF)?;
        match token.kind {
            TokenKind::Dash => {
                self.cursor += 1;
                Ok(())
            }
            _ => Err(ParserError::Unexpected(
                token.clone(),
                Expected(vec![TokenKind::Dash]),
            )),
        }
    }
    fn slash(&mut self) -> Result<(), ParserError> {
        let token = self
            .string
            .get(self.cursor)
            .ok_or(ParserError::UnexpectedEOF)?;
        match token.kind {
            TokenKind::Slash => {
                self.cursor += 1;
                Ok(())
            }
            _ => Err(ParserError::Unexpected(
                token.clone(),
                Expected(vec![TokenKind::Slash]),
            )),
        }
    }
    fn positions(&mut self) -> Result<[Option<board::Piece>; 64], ParserError> {
        let mut board: [Option<board::Piece>; 64] = [None; 64];
        for rank in (0..8).rev() {
            if rank != 7 {
                self.slash()?;
            }
            let mut file = 0;
            while file < 8 {
                match self.piece() {
                    Ok(piece) => {
                        board[rank * 8 + file] = Some(piece);
                        file += 1;
                    }
                    Err(ParserError::Unexpected(_, ep)) => {
                        let num = self.number().or_else(|x| {
                            Err(match x {
                                ParserError::Unexpected(t, e) => {
                                    ParserError::Unexpected(t, e.append(ep))
                                }
                                x => x,
                            })
                        })?;
                        file += num as usize;
                        if file < 1 || file > 8 {
                            return Err(ParserError::FileOutOfBounds(num.into()));
                        }
                    }
                    Err(x) => return Err(x),
                };
            }
        }
        Ok(board)
    }
    fn player(&mut self) -> Result<board::Player, ParserError> {
        let token = self
            .string
            .get(self.cursor)
            .ok_or(ParserError::UnexpectedEOF)?;
        match token.kind {
            TokenKind::Color => {
                self.cursor += 1;
                Ok(if token.lexme == "w" {
                    board::Player::White
                } else {
                    board::Player::Black
                })
            }
            _ => Err(ParserError::Unexpected(
                token.clone(),
                Expected(vec![TokenKind::Color]),
            )),
        }
    }

    fn castling_rights(&mut self) -> Result<(bool, bool, bool, bool), ParserError> {
        let expected = match self.dash() {
            Ok(_) => return Ok((false, false, false, false)),
            Err(ParserError::Unexpected(_, e)) => e.append(Expected(vec![TokenKind::Piece])),
            Err(x) => return Err(x),
        };
        let mut end = 0;
        let mut black_king = false;
        let mut black_queen = false;
        let mut white_king = false;
        let mut white_queen = false;
        for i in self
            .string
            .get(self.cursor..)
            .ok_or(ParserError::UnexpectedEOF)?
        {
            if end >= 4 {
                break;
            }
            match i.kind {
                TokenKind::Piece => match i.lexme.as_str() {
                    "k" => black_king = true,
                    "q" => black_queen = true,
                    "K" => white_king = true,
                    "Q" => white_queen = true,
                    _ => {
                        if end == 0 {
                            return Err(ParserError::InvalidCastlingSide(i.clone()));
                        }
                        break;
                    }
                },
                _ => {
                    if end == 0 {
                        return Err(ParserError::Unexpected(i.clone(), expected));
                    }
                    break;
                }
            }
            end += 1;
        }
        self.cursor += end;
        return Ok((white_queen, white_king, black_queen, black_king));
    }
    fn en_passent(&mut self) -> Result<Option<board::Square>, ParserError> {
        let expected = match self.dash() {
            Ok(_) => return Ok(None),
            Err(ParserError::Unexpected(_, e)) => e.append(Expected(vec![TokenKind::Square])),
            Err(x) => return Err(x),
        };
        let tok = self
            .string
            .get(self.cursor)
            .ok_or(ParserError::UnexpectedEOF)?;
        match tok.kind {
            TokenKind::Square => {
                let mut square = tok.lexme.chars();
                let file = (square.next().unwrap() as u8) - 97;
                let rank = square.next().unwrap().to_digit(10).unwrap() - 1;
                self.cursor += 1;
                Ok(Some(board::Square(rank as u8, file)))
            }
            _ => Err(ParserError::Unexpected(tok.clone(), expected)),
        }
    }
    fn number(&mut self) -> Result<u16, ParserError> {
        let tok = self
            .string
            .get(self.cursor)
            .ok_or(ParserError::UnexpectedEOF)?;
        match tok.kind {
            TokenKind::Number => {
                self.cursor += 1;
                Ok(tok.lexme.parse().unwrap())
            }
            _ => Err(ParserError::Unexpected(
                tok.clone(),
                Expected(vec![TokenKind::Number]),
            )),
        }
    }
    fn whitespace(&mut self) -> Result<(), ParserError> {
        let tok = self
            .string
            .get(self.cursor)
            .ok_or(ParserError::UnexpectedEOF)?;
        match tok.kind {
            TokenKind::Whitespace => {
                self.cursor += 1;
                Ok(())
            }
            _ => Err(ParserError::Unexpected(
                tok.clone(),
                Expected(vec![TokenKind::Whitespace]),
            )),
        }
    }
    fn new(string: Vec<Rc<Token>>) -> Self {
        Self { string, cursor: 0 }
    }
    fn run(&mut self) -> Result<board::Board, ParserError> {
        let positions = self.positions()?;
        self.whitespace()?;
        let turn = self.player()?;
        self.whitespace()?;
        let castling_rights = self.castling_rights()?;
        self.whitespace()?;
        let en_passent = self.en_passent()?;
        self.whitespace()?;
        let reversible_moves = self.number()?;
        self.whitespace()?;
        let full_moves = self.number()?;
        Ok(board::Board {
            positions,
            turn,
            castling_rights,
            en_passent,
            reversible_moves,
            full_moves,
        })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unknown token on: {0}")]
    LexerError(usize),
    #[error("parser error: {0}")]
    ParserError(#[from] ParserError),
}

pub fn parse(string: &str) -> Result<board::Board, Error> {
    let mut lexer = Lexer::new(string);
    let toks = lexer
        .run()
        .ok_or_else(move || Error::LexerError(lexer.cursor))?;
    let mut parser = Parser::new(toks);
    Ok(parser.run()?)
}

fn piece_to_fen(piece: board::Piece) -> char {
    let p = match piece.kind {
        board::PieceKind::Pawn => 'p',
        board::PieceKind::Rook => 'r',
        board::PieceKind::Knight => 'n',
        board::PieceKind::Bishop => 'b',
        board::PieceKind::Queen => 'q',
        board::PieceKind::King => 'k',
    };
    match piece.owner {
        board::Player::White => p.to_ascii_uppercase(),
        board::Player::Black => p,
    }
}

pub fn board_to_fen(b: board::Board) -> String {
    let mut ret = String::new();
    for rank in (0..8).rev() {
        let mut nonec = 0;
        if rank != 7 {
            ret.push('/');
        }
        for file in 0..8 {
            if let Some(piece) = b.positions[rank * 8 + file] {
                if nonec != 0 {
                    ret.push_str(&nonec.to_string());
                    nonec = 0;
                }
                ret.push(piece_to_fen(piece));
            } else {
                nonec += 1;
            }
        }
        if nonec != 0 {
            ret.push_str(&nonec.to_string());
        }
    }
    ret.push(' ');
    ret.push(match b.turn {
        board::Player::White => 'w',
        board::Player::Black => 'b',
    });
    ret.push(' ');
    let (white_queen, white_king, black_queen, black_king) = b.castling_rights;
    if !white_queen && !white_king && !black_queen && !black_king {
        ret.push('-');
    } else {
        if white_king {
            ret.push('K');
        }
        if white_queen {
            ret.push('Q');
        }
        if black_king {
            ret.push('k');
        }
        if black_queen {
            ret.push('q');
        }
    }
    ret.push(' ');
    if let Some(square) = b.en_passent {
        ret.push_str(&square.to_string());
    } else {
        ret.push('-');
    }
    ret.push(' ');
    ret.push_str(&b.reversible_moves.to_string());
    ret.push(' ');
    ret.push_str(&b.full_moves.to_string());
    ret
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let s = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e3 0 1";
        let p = parse(s).unwrap();
        assert_eq!(board_to_fen(p), s);
        let s = "rnbqkbnr/1pp1p1pp/8/p2pPp2/3P4/8/PPP2PPP/RNBQKBNR w KQkq f6 0 4";
        let p = parse(s).unwrap();
        assert_eq!(board_to_fen(p), s);
        let s = "rnbq1rk1/1pp1n1pp/5p2/p7/1bpP4/5P2/PPPBN1PP/RN1Q1RK1 b - - 1 9";
        let p = parse(s).unwrap();
        assert_eq!(board_to_fen(p), s);
        let s = "rnbqkbnr/pppp2pp/4p3/8/2BP4/5p2/PPP2PPP/RNBQ1RK1 b kq - 1 5";
        let p = parse(s).unwrap();
        assert_eq!(board_to_fen(p), s);
        let s = "rnbqk1r1/pppp3p/3bpnp1/8/2BP4/5P1K/PPP2P1P/RNBQ1R2 w q - 4 9";
        let p = parse(s).unwrap();
        assert_eq!(board_to_fen(p), s);
    }
}
