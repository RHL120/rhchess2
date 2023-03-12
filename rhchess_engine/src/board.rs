/// The kind of a chess peice
#[derive(Debug, Clone, Copy)]
pub enum PieceKind {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

/// The 2 possible players
#[derive(Debug, Clone, Copy)]
pub enum Player {
    White,
    Black,
}

/// The piece itself
#[derive(Debug, Clone, Copy)]
pub struct Piece {
    pub kind: PieceKind,
    pub owner: Player,
}

pub struct Square(pub u8, pub u8);

impl Square {
    pub fn new(file: u8, rank: u8) -> Option<Square> {
        if file < 8 && rank < 8 {
            Some(Square(file, rank))
        } else {
            None
        }
    }
}

impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let ranks = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
        write!(f, "{}{}", ranks[self.1 as usize], self.0 + 1)
    }
}

impl std::fmt::Debug for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Display::fmt(self, f)
    }
}

/// The board representation
#[derive(Debug)]
pub struct Board {
    /// The piece placement
    pub positions: [Option<Piece>; 64],
    /// The player that should play in the current move
    pub turn: Player,
    /// (white queen castle, white king castle, black queen castle, black king castle)
    pub castling_rights: (bool, bool, bool, bool),
    /// The en passent target square
    pub en_passent: Option<Square>,
    /// The number of moves that does not envolve a capture/pawn push
    pub reversible_moves: u16,
    /// The number of moves
    pub full_moves: u16,
}

pub enum BoardStringKind {
    Fen,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("fen error: {0}")]
    FenError(#[from] crate::fen::Error),
}

impl Board {
    pub fn new(s: &str, kind: &BoardStringKind) -> Result<Self, Error> {
        Ok(match kind {
            BoardStringKind::Fen => crate::fen::parse(s)?,
        })
    }
}
