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

/// The board representation
#[derive(Debug, Clone, Copy)]
pub struct Board {
    /// The piece placement
    pub positions: [Option<Piece>; 64],
    /// The player that should play in the current move
    pub turn: Player,
    /// (white queen castle, white king castle, black queen castle, black king castle)
    pub castling_rights: (bool, bool, bool, bool),
    /// The en passent target square
    pub en_passent: Option<(u8, u8)>,
    /// The number of moves that does not envolve a capture/pawn push
    pub reversible_moves: u16,
    /// The number of moves
    pub full_moves: u16,
}
