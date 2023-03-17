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
    pub fn opposite(&self) -> Self {
        match self {
            Player::White => Player::Black,
            Player::Black => Player::White,
        }
    }
    pub fn king_rank(&self) -> u8 {
        match self {
            Player::White => 0,
            Player::Black => 7,
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

#[derive(Debug)]
pub struct CastlingRights {
    pub white_queen: bool,
    pub white_king: bool,
    pub black_queen: bool,
    pub black_king: bool,
}

impl CastlingRights {
    fn set(&mut self, queen: Option<bool>, king: Option<bool>, player: Player) {
        match player {
            Player::Black => {
                self.black_king = king.unwrap_or(self.black_king);
                self.black_queen = queen.unwrap_or(self.black_queen);
            }
            Player::White => {
                self.white_king = king.unwrap_or(self.white_king);
                self.white_queen = queen.unwrap_or(self.white_queen);
            }
        }
    }
}

impl Default for CastlingRights {
    fn default() -> Self {
        CastlingRights {
            white_queen: true,
            white_king: true,
            black_queen: true,
            black_king: true,
        }
    }
}

/// The board representation
#[derive(Debug)]
pub struct Board {
    /// The piece placement
    pub positions: [Option<Piece>; 64],
    /// The player that should play in the current move
    pub turn: Player,
    pub castling_rights: CastlingRights,
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
    fn update_castling(&mut self, m: moves::Move) {
        match m {
            moves::Move::Move(_, dst, src) => {
                match self.get_piece(src).unwrap().kind {
                    PieceKind::Rook => {
                        //no need to check the rank because if it has changed the player
                        //has already lost their castling rights on that rook
                        match src.file {
                            7 => self.castling_rights.set(None, Some(false), self.turn),
                            0 => self.castling_rights.set(Some(false), None, self.turn),
                            _ => (),
                        };
                    }
                    PieceKind::King => {
                        self.castling_rights
                            .set(Some(false), Some(false), self.turn);
                        //If the king is close to atcking an enemy rook
                        //it has already lost it's castling rights so no need
                        //to check for that.
                        return;
                    }
                    _ => (),
                }
                let opposite_rank = match self.turn {
                    Player::Black => 0,
                    Player::White => 7,
                };
                if dst == Square::new(7, opposite_rank).unwrap() {
                    self.castling_rights
                        .set(None, Some(false), self.turn.opposite());
                } else if dst == Square::new(0, opposite_rank).unwrap() {
                    self.castling_rights
                        .set(Some(false), None, self.turn.opposite());
                }
            }
            moves::Move::Castle(_) => {
                self.castling_rights
                    .set(Some(false), Some(false), self.turn);
            }
            _ => (),
        }
    }
    pub fn make_move(&mut self, m: moves::Move) {
        self.update_castling(m);
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
            moves::Move::Castle(king_side) => {
                let rank = match self.turn {
                    Player::White => 0,
                    Player::Black => 7,
                };
                if king_side {
                    self.positions[rank * 8 + 6] = self.positions[rank * 8 + 4];
                    self.positions[rank * 8 + 4] = None;
                    self.positions[rank * 8 + 5] = self.positions[rank * 8 + 7];
                    self.positions[rank * 8 + 7] = None;
                } else {
                    self.positions[rank * 8 + 2] = self.positions[rank * 8 + 4];
                    self.positions[rank * 8 + 4] = None;
                    self.positions[rank * 8 + 3] = self.positions[rank * 8];
                    self.positions[rank * 8] = None;
                }
            }
        };
    }
    pub fn switch_player(&mut self) {
        self.turn = self.turn.opposite();
    }
    pub fn is_promotion(&self, m: moves::Move) -> bool {
        match m {
            moves::Move::Move(_, dst, src) => match self.get_piece(src).unwrap().kind {
                PieceKind::Pawn => dst.rank == self.turn.opposite().king_rank(),
                _ => false,
            },
            _ => false,
        }
    }
    pub fn make_promotion(&mut self, m: moves::Move, kind: PieceKind) {
        match m {
            moves::Move::Move(_, dst, _) => {
                self.positions[(8 * dst.rank + dst.file) as usize] = Some(Piece {
                    kind,
                    owner: self.turn,
                })
            }
            _ => unreachable!(),
        }
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
            castling_rights: CastlingRights::default(),
            en_passant: None,
            reversible_moves: 0,
            full_moves: 1,
        }
    }
}
