use crate::moves;
/// The kind of a chess peice
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceKind {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

/// The 2 possible players
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Player {
    White,
    Black,
}

impl Player {
    pub fn pawn_info(&self) -> (u8, i32) {
        match self {
            Player::White => (1, 1),
            Player::Black => (6, -1),
        }
    }
}

/// The piece itself
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Piece {
    pub kind: PieceKind,
    pub owner: Player,
}

#[derive(Clone, Copy, PartialEq)]
pub struct Square {
    pub rank: u8,
    pub file: u8,
}

impl Square {
    pub fn new(file: u8, rank: u8) -> Option<Square> {
        if file < 8 && rank < 8 {
            Some(Square { file, rank })
        } else {
            None
        }
    }
    pub fn from_idx(idx: u8) -> Option<Square> {
        if idx < 64 {
            Some(Square {
                rank: idx / 8,
                file: idx % 8,
            })
        } else {
            None
        }
    }
    pub fn translate(self, file: i32, rank: i32) -> Option<Square> {
        let file = self.file as i32 + file;
        let rank = self.rank as i32 + rank;
        if file < 8 && rank < 8 {
            Some(Square {
                rank: rank.try_into().ok()?,
                file: file.try_into().ok()?,
            })
        } else {
            None
        }
    }
}

impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let files = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
        write!(f, "{}{}", files[self.file as usize], self.rank + 1)
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
    /// The en passant target square
    pub en_passant: Option<Square>,
    /// The number of moves that does not envolve a capture/pawn push
    pub reversible_moves: u16,
    /// The number of moves
    pub full_moves: u16,
}

impl Board {
    pub fn get_piece(&self, s: Square) -> Option<Piece> {
        self.positions[(s.rank * 8 + s.file) as usize]
    }
    pub fn make_move(&mut self, m: moves::Move) {
        match m {
            moves::Move::Move(_, dst, src) => {
                let piece = self.positions[(src.rank * 8 + src.file) as usize].unwrap();
                self.en_passant = if let PieceKind::Pawn = piece.kind {
                    let (init_rank, _) = piece.owner.pawn_info();
                    if src.rank == init_rank && ((dst.rank as i32) - (src.rank as i32)).abs() == 2 {
                        Some(dst)
                    } else {
                        None
                    }
                } else {
                    None
                };
                self.positions[(src.rank * 8 + src.file) as usize] = None;
                self.positions[(dst.rank * 8 + dst.file) as usize] = Some(piece);
            }
            moves::Move::EnPassent(src) => {
                let piece = self.positions[(src.rank * 8 + src.file) as usize].unwrap();
                self.positions[(src.rank * 8 + src.file) as usize] = None;
                let en_passant = self.en_passant.unwrap();
                let dst = en_passant.translate(0, piece.owner.pawn_info().1).unwrap();
                self.positions[(en_passant.rank * 8 + en_passant.file) as usize] = None;
                self.positions[(dst.rank * 8 + dst.file) as usize] = Some(piece);
                self.en_passant = None;
            }
            _ => todo!(),
        };
    }
    pub fn switch_player(&mut self) {
        self.turn = match self.turn {
            Player::White => Player::Black,
            Player::Black => Player::White,
        };
    }
}

impl Default for Board {
    fn default() -> Self {
        use PieceKind::*;
        use Player::*;
        let positions: [Option<Piece>; 64] = [
            Some(Piece {
                kind: Rook,
                owner: White,
            }),
            Some(Piece {
                kind: Knight,
                owner: White,
            }),
            Some(Piece {
                kind: Bishop,
                owner: White,
            }),
            Some(Piece {
                kind: Queen,
                owner: White,
            }),
            Some(Piece {
                kind: King,
                owner: White,
            }),
            Some(Piece {
                kind: Bishop,
                owner: White,
            }),
            Some(Piece {
                kind: Knight,
                owner: White,
            }),
            Some(Piece {
                kind: Rook,
                owner: White,
            }),
            Some(Piece {
                kind: Pawn,
                owner: White,
            }),
            Some(Piece {
                kind: Pawn,
                owner: White,
            }),
            Some(Piece {
                kind: Pawn,
                owner: White,
            }),
            Some(Piece {
                kind: Pawn,
                owner: White,
            }),
            Some(Piece {
                kind: Pawn,
                owner: White,
            }),
            Some(Piece {
                kind: Pawn,
                owner: White,
            }),
            Some(Piece {
                kind: Pawn,
                owner: White,
            }),
            Some(Piece {
                kind: Pawn,
                owner: White,
            }),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(Piece {
                kind: Pawn,
                owner: Black,
            }),
            Some(Piece {
                kind: Pawn,
                owner: Black,
            }),
            Some(Piece {
                kind: Pawn,
                owner: Black,
            }),
            Some(Piece {
                kind: Pawn,
                owner: Black,
            }),
            Some(Piece {
                kind: Pawn,
                owner: Black,
            }),
            Some(Piece {
                kind: Pawn,
                owner: Black,
            }),
            Some(Piece {
                kind: Pawn,
                owner: Black,
            }),
            Some(Piece {
                kind: Pawn,
                owner: Black,
            }),
            Some(Piece {
                kind: Rook,
                owner: Black,
            }),
            Some(Piece {
                kind: Knight,
                owner: Black,
            }),
            Some(Piece {
                kind: Bishop,
                owner: Black,
            }),
            Some(Piece {
                kind: Queen,
                owner: Black,
            }),
            Some(Piece {
                kind: King,
                owner: Black,
            }),
            Some(Piece {
                kind: Bishop,
                owner: Black,
            }),
            Some(Piece {
                kind: Knight,
                owner: Black,
            }),
            Some(Piece {
                kind: Rook,
                owner: Black,
            }),
        ];
        Self {
            positions,
            turn: White,
            castling_rights: (true, true, true, true),
            en_passant: None,
            reversible_moves: 0,
            full_moves: 1,
        }
    }
}
