use crate::fen;
use crate::moves;
use std::collections::HashMap;
use std::collections::HashSet;
/// The kind of a chess peice
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceKind {
    ///The pawn
    Pawn,
    ///The rook
    Rook,
    ///The knight
    Knight,
    ///The bishop
    Bishop,
    ///The queen
    Queen,
    ///The king
    King,
}

/// The 2 possible players
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Player {
    /// white
    White,
    /// black
    Black,
}

impl Player {
    /// The first return value is the rank that the pawns start at for `self`.
    /// The second is the direction in which they move
    pub fn pawn_info(&self) -> (u8, i32) {
        match self {
            Player::White => (1, 1),
            Player::Black => (6, -1),
        }
    }
    /// For self == White, return is Black
    /// For self == Black, return is White
    pub fn opposite(&self) -> Self {
        match self {
            Player::White => Player::Black,
            Player::Black => Player::White,
        }
    }
    /// Returns the rank on which the king starts on
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
    ///The kind of the piece
    pub kind: PieceKind,
    ///The piece's color
    pub owner: Player,
}

/// A board square
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Square {
    pub rank: u8,
    pub file: u8,
}

impl Square {
    /// Creates a new square
    pub fn new(file: u8, rank: u8) -> Option<Square> {
        if file < 8 && rank < 8 {
            Some(Square { file, rank })
        } else {
            None
        }
    }
    /// Creates a new square where file and rank are known to be less than 8
    pub fn new_unsafe(file: u8, rank: u8) -> Square {
        Square { file, rank }
    }
    /// Creates a new square from an index where idx is known to be less than 64
    pub fn from_idx_unsafe(idx: u8) -> Square {
        Square {
            rank: idx / 8,
            file: idx % 8,
        }
    }
    /// Creates a new square from an index where idx is known to be less than 64
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
    /// Returns a new square that is Square {file: self.file + file, rank: self.rank + rank}
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
    /// Generates a line that passes through square
    pub fn generate_lines(self, diffs: &[(i32, i32)]) -> Vec<Line> {
        diffs
            .iter()
            .map(|&(df, dr)| {
                (1..8)
                    .map_while(move |x| self.translate(df * x, dr * x))
                    .collect()
            })
            .collect()
    }
    /// Generates the line between self and other starting from self
    pub fn between(self, other: Self) -> Line {
        let rank_diff = (other.rank as i32 - self.rank as i32).signum();
        let file_diff = (other.file as i32 - self.file as i32).signum();
        (1..8)
            .map_while(move |x| {
                let sq = self.translate(file_diff * x, rank_diff * x)?;
                if sq == other {
                    None
                } else {
                    Some(sq)
                }
            })
            .collect()
    }
}

/// A list of squares that should be aligned
pub type Line = Vec<Square>;

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

#[derive(Debug, Clone)]
/// What players have the right to castle their king and from what side
pub struct CastlingRights {
    /// white can castle queen side
    pub white_queen: bool,
    /// white can castle king side
    pub white_king: bool,
    /// black can castle queen side
    pub black_queen: bool,
    /// black can castle king side
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

#[derive(Debug, Default, Clone)]
/// Keeps track of pinned pieces, attacked squares and en passent checks
pub struct Attacks {
    /// The key contains the squares that white attacks and the value contains
    /// the squares from which white attacks them
    pub white_attacks: HashMap<Square, HashSet<Square>>,
    /// The key contains the squares that black attacks and the value contains
    /// the squares from which black attacks them
    pub black_attacks: HashMap<Square, HashSet<Square>>,
    /// The key contains squares that white pins and the value contains the square
    /// that contains the pinner
    pub white_pins: HashMap<Square, Square>,
    /// The key contains squares that black pins and the value contains the square
    /// that contains the pinner
    pub black_pins: HashMap<Square, Square>,
}

impl Attacks {
    /// Removes all attacks from `square`
    fn clear_attacks_by(&mut self, square: Square, player: Player) {
        let attacks = match player {
            Player::White => &mut self.white_attacks,
            Player::Black => &mut self.black_attacks,
        };
        let mut to_delete = Vec::new();
        for (key, value) in &mut *attacks {
            value.remove(&square);
            if value.is_empty() {
                to_delete.push(*key);
            }
        }
        for i in to_delete {
            attacks.remove(&i);
        }
    }
    /// Get's the attacks done by the player `p`
    pub fn get_attacks_for(&self, p: Player, s: Square) -> Option<&HashSet<Square>> {
        match p {
            Player::White => self.white_attacks.get(&s),
            Player::Black => self.black_attacks.get(&s),
        }
    }
    /// Checks if the player `p` attacks the square `s`
    pub fn does_attack(&self, p: Player, s: Square) -> bool {
        self.get_attacks_for(p, s).is_some()
    }
    /// Returns the square of the piece pinning the piece on `s`
    pub fn get_pinner(&self, p: Player, s: Square) -> Option<Square> {
        match p {
            Player::White => self.black_pins.get(&s).copied(),
            Player::Black => self.white_pins.get(&s).copied(),
        }
    }
    /// Clear all the squares pinned by the player `p`
    fn clear_pins_by(&mut self, p: Player, s: Square) {
        let pins = match p {
            Player::White => &mut self.white_pins,
            Player::Black => &mut self.black_pins,
        };
        let pos = pins
            .iter()
            .find_map(|(&k, &v)| if s == v { Some(k) } else { None });
        if let Some(pos) = pos {
            pins.remove(&pos);
        }
    }
}
/// The board representation
#[derive(Debug, Clone)]
pub struct Board {
    /// the position of the black king
    pub black_pos: Square,
    /// the position of the white king
    pub white_pos: Square,
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
    /// The attack information (relevant to checks)
    pub attacks: Attacks,
}

impl Board {
    /// Gets the king position for the current player (`self.turn`)
    pub fn current_king(&self) -> Square {
        match self.turn {
            Player::White => self.white_pos,
            Player::Black => self.black_pos,
        }
    }
    /// Gets the piece found on the square `s`
    pub fn get_piece(&self, s: Square) -> Option<Piece> {
        self.positions[(s.rank * 8 + s.file) as usize]
    }
    /// Set the square `s` to the piece `p`
    fn set_piece(&mut self, s: Square, p: Option<Piece>) {
        self.positions[(s.rank * 8 + s.file) as usize] = p;
    }
    /// Updates the `self.castling_rights` based on the move `m`
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
                        //If the king is close to attacking an enemy rook
                        //it has already lost it's castling rights so no need
                        //to check for that.
                        return;
                    }
                    _ => (),
                }
                let opposite_rank = self.turn.opposite().king_rank();
                if dst == Square::new_unsafe(7, opposite_rank) {
                    self.castling_rights
                        .set(None, Some(false), self.turn.opposite());
                } else if dst == Square::new_unsafe(0, opposite_rank) {
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
    /// Applies the move `m` on the board
    pub fn make_move(&mut self, m: moves::Move) {
        self.update_castling(m);
        match m {
            moves::Move::Move(_, dst, src) => {
                let piece = self.get_piece(src).unwrap();
                if piece.kind == PieceKind::King {
                    match self.turn {
                        Player::Black => self.black_pos = dst,
                        Player::White => self.white_pos = dst,
                    };
                }
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
                self.set_piece(src, None);
                let capt = self.get_piece(dst);
                self.set_piece(dst, Some(piece));
                self.update_attacks(m, capt.map(|x| (x, dst)))
            }
            moves::Move::EnPassent(src) => {
                let piece = self.get_piece(src).unwrap();
                self.set_piece(src, None);
                let en_passant = self.en_passant.unwrap();
                let dst = en_passant.translate(0, piece.owner.pawn_info().1).unwrap();
                let capt = self.get_piece(en_passant);
                self.set_piece(dst, Some(piece));
                self.set_piece(en_passant, None);
                self.update_attacks(m, capt.map(|x| (x, en_passant)));
                self.en_passant = None;
            }
            moves::Move::Castle(king_side) => {
                let rank = match self.turn {
                    Player::White => 0,
                    Player::Black => 7,
                };
                if king_side {
                    match self.turn {
                        Player::Black => {
                            self.black_pos = Square {
                                rank: rank as u8,
                                file: 6,
                            }
                        }
                        Player::White => {
                            self.white_pos = Square {
                                rank: rank as u8,
                                file: 6,
                            }
                        }
                    };
                    self.positions[rank * 8 + 6] = self.positions[rank * 8 + 4];
                    self.positions[rank * 8 + 4] = None;
                    self.positions[rank * 8 + 5] = self.positions[rank * 8 + 7];
                    self.positions[rank * 8 + 7] = None;
                } else {
                    match self.turn {
                        Player::Black => {
                            self.black_pos = Square {
                                rank: rank as u8,
                                file: 2,
                            }
                        }
                        Player::White => {
                            self.white_pos = Square {
                                rank: rank as u8,
                                file: 2,
                            }
                        }
                    };
                    self.positions[rank * 8 + 2] = self.positions[rank * 8 + 4];
                    self.positions[rank * 8 + 4] = None;
                    self.positions[rank * 8 + 3] = self.positions[rank * 8];
                    self.positions[rank * 8] = None;
                }
                self.update_attacks(m, None);
            }
        };
    }
    /// Moves to the next turn
    pub fn switch_player(&mut self) {
        self.turn = self.turn.opposite();
    }
    /// Checks if the move `m` results in a promotion
    pub fn is_promotion(&self, m: moves::Move) -> bool {
        match m {
            moves::Move::Move(_, dst, src) => match self.get_piece(src).unwrap().kind {
                PieceKind::Pawn => dst.rank == self.turn.opposite().king_rank(),
                _ => false,
            },
            _ => false,
        }
    }
    /// Makes the promoition based on `m` to the kind `kind`
    pub fn make_promotion(&mut self, m: moves::Move, kind: PieceKind) {
        match m {
            moves::Move::Move(_, dst, _) => {
                let p = Piece {
                    kind,
                    owner: self.turn,
                };
                self.set_piece(dst, Some(p));
                self.calculate_piece_attack(dst, p);
            }
            _ => unreachable!(),
        }
    }
    /// Checks if the first piece in the line `rng` is one of `kinds`
    fn first_piece_is(&self, rng: &Line, kinds: &[PieceKind]) -> Option<(Square, Piece)> {
        for &i in rng {
            if let Some(piece) = self.get_piece(i) {
                if kinds.contains(&piece.kind) {
                    return Some((i, piece));
                } else {
                    return None;
                }
            }
        }
        None
    }
    /// Returns all of the pieces and squares that are effected by a move by
    /// the square sq
    fn surrounding_pieces(&mut self, sq: Square) -> Vec<(Square, Piece)> {
        use moves::{BISHOP_TRANSLATES, KING_TRANSLATES, KNIGHT_TRANSLATES, ROOK_TRANSLATES};
        use PieceKind::{Bishop, King, Knight, Queen, Rook};
        let mut ret = Vec::new();
        for (translates, kind) in [(ROOK_TRANSLATES, Rook), (BISHOP_TRANSLATES, Bishop)] {
            let mut pieces = translates
                .iter()
                .filter_map(|&ds| {
                    self.first_piece_is(sq.generate_lines(&[ds]).first().unwrap(), &[Queen, kind])
                })
                .collect();
            ret.append(&mut pieces);
        }
        for (translates, kind) in [(KNIGHT_TRANSLATES, Knight), (KING_TRANSLATES, King)] {
            let mut pieces =
                translates
                    .iter()
                    .filter_map(|&(df, dr)| {
                        let sq = sq.translate(df, dr)?;
                        self.get_piece(sq).and_then(|p| {
                            if p.kind == kind {
                                Some((sq, p))
                            } else {
                                None
                            }
                        })
                    })
                    .collect();
            ret.append(&mut pieces);
        }
        ret
    }
    /// Updatest the attacks of the knight on the square `square`
    fn update_knight_attacks(&mut self, square: Square, p: Player) {
        let r = match p {
            Player::White => &mut self.attacks.white_attacks,
            Player::Black => &mut self.attacks.black_attacks,
        };
        let attacks = moves::KNIGHT_TRANSLATES
            .iter()
            .filter_map(|&(df, dr)| square.translate(df, dr));
        for i in attacks {
            r.entry(i).or_insert_with(HashSet::new);
            r.get_mut(&i).unwrap().insert(square);
        }
    }
    /// Updatest the attacks of the king on the square `square`
    fn update_king_attacks(&mut self, square: Square, p: Player) {
        let r = match p {
            Player::White => &mut self.attacks.white_attacks,
            Player::Black => &mut self.attacks.black_attacks,
        };
        let attacks = moves::KING_TRANSLATES
            .iter()
            .filter_map(|&(df, dr)| square.translate(df, dr));
        for i in attacks {
            r.entry(i).or_insert_with(HashSet::new);
            r.get_mut(&i).unwrap().insert(square);
        }
        self.on_king_update_pins(square)
    }
    /// Filters out the attacked squares from lines
    fn to_attacks(&self, lines: Vec<Line>) -> Vec<HashSet<Square>> {
        let mut ret = Vec::new();
        for line in lines {
            let mut h = HashSet::new();
            for square in line {
                match self.get_piece(square) {
                    None => h.insert(square),
                    Some(_) => {
                        h.insert(square);
                        break;
                    }
                };
            }
            ret.push(h)
        }
        ret
    }
    /// Gets the pinner square (if there is one)
    fn get_pin(
        &self,
        sq: Square,
        diff_check: impl Fn(i32, i32) -> bool,
        c: Player,
    ) -> Option<Square> {
        let opp_king_pos = match c {
            Player::White => self.black_pos,
            Player::Black => self.white_pos,
        };
        let rank_diff = opp_king_pos.rank as i32 - sq.rank as i32;
        let file_diff = opp_king_pos.file as i32 - sq.file as i32;
        if diff_check(rank_diff, file_diff) {
            let rank_diff = rank_diff.signum();
            let file_diff = file_diff.signum();
            let seps: Vec<(Square, Piece)> = (1..8)
                .map_while(|x| {
                    let sq = sq
                        .translate(x * file_diff, x * rank_diff)
                        .expect("While looking for a pinned piece, reached edge before king");
                    if sq == opp_king_pos {
                        None
                    } else {
                        Some((sq, self.get_piece(sq)))
                    }
                })
                .filter_map(|(sq, p)| Some((sq, p?)))
                .collect();
            match seps.len() {
                1 => Some(seps.first().unwrap().0),
                _ => None,
            }
        } else {
            None
        }
    }
    /// Updates the attacks for the bishop found on `square`
    fn update_bishop_attacks(&mut self, square: Square, p: Player) {
        let attacks = self.to_attacks(square.generate_lines(&moves::BISHOP_TRANSLATES));
        if let Some(pin) = self.get_pin(square, |x, y| x.abs() == y.abs(), p) {
            match p {
                Player::White => self.attacks.white_pins.insert(pin, square),
                Player::Black => self.attacks.black_pins.insert(pin, square),
            };
        }
        let r = match p {
            Player::White => &mut self.attacks.white_attacks,
            Player::Black => &mut self.attacks.black_attacks,
        };
        for i in attacks {
            for j in i {
                r.entry(j).or_insert_with(HashSet::new);
                r.get_mut(&j).unwrap().insert(square);
            }
        }
    }
    /// Updates the attacks for the rook found on `square`
    fn update_rook_attacks(&mut self, square: Square, p: Player) {
        let attacks = self.to_attacks(square.generate_lines(&[(1, 0), (0, 1), (-1, 0), (0, -1)]));
        if let Some(pin) = self.get_pin(square, |x, y| x == 0 || y == 0, p) {
            match p {
                Player::White => self.attacks.white_pins.insert(pin, square),
                Player::Black => self.attacks.black_pins.insert(pin, square),
            };
        }
        let r = match p {
            Player::White => &mut self.attacks.white_attacks,
            Player::Black => &mut self.attacks.black_attacks,
        };
        for i in attacks {
            for j in i {
                r.entry(j).or_insert_with(HashSet::new);
                r.get_mut(&j).unwrap().insert(square);
            }
        }
    }
    /// Updates the attacks for the pawn found on `square`
    fn update_pawn_attacks(&mut self, square: Square, p: Player) {
        let r = match p {
            Player::White => &mut self.attacks.white_attacks,
            Player::Black => &mut self.attacks.black_attacks,
        };
        let (_, rank_multiple) = p.pawn_info();
        for i in [-1, 1] {
            if let Some(i) = square.translate(i, rank_multiple) {
                r.entry(i).or_insert_with(HashSet::new);
                r.get_mut(&i).unwrap().insert(square);
            }
        }
    }
    /// Updates the attacks for the queen found on `square`
    fn update_queen_attacks(&mut self, square: Square, p: Player) {
        self.update_rook_attacks(square, p);
        self.update_bishop_attacks(square, p);
    }
    /// Updates the attacks for the piece found on `square`
    fn calculate_piece_attack(&mut self, square: Square, p: Piece) {
        let pl = p.owner;
        match p.kind {
            PieceKind::Rook => {
                self.attacks.clear_pins_by(p.owner, square);
                self.update_rook_attacks(square, pl)
            }
            PieceKind::Queen => {
                self.attacks.clear_pins_by(p.owner, square);
                self.update_queen_attacks(square, pl)
            }
            PieceKind::Bishop => {
                self.attacks.clear_pins_by(p.owner, square);
                self.update_bishop_attacks(square, pl)
            }
            PieceKind::Knight => self.update_knight_attacks(square, pl),
            PieceKind::King => {
                self.on_king_update_pins(square);
                self.update_king_attacks(square, pl)
            }
            PieceKind::Pawn => self.update_pawn_attacks(square, pl),
        };
    }
    fn pinner_pinned(
        &self,
        rng: &Line,
        kind: PieceKind,
        color: Player,
    ) -> Option<(Square, Square)> {
        let mut pinner = None;
        let mut pinned = None;
        for &i in rng {
            if let Some(piece) = self.get_piece(i) {
                if piece.owner == color {
                    if pinned.is_some() {
                        break;
                    }
                    pinned = Some(i);
                } else {
                    pinned?;
                    if kind == piece.kind || piece.kind == PieceKind::Queen {
                        pinner = Some(i);
                    }
                    break;
                }
            }
        }
        Some((pinner?, pinned?))
    }
    fn on_king_update_pins(&mut self, sq: Square) {
        use moves::{BISHOP_TRANSLATES, ROOK_TRANSLATES};
        use PieceKind::{Bishop, Rook};
        let color = self.get_piece(sq).unwrap().owner;
        let mut ret: Vec<(Square, Square)> = Vec::new();
        for (translate, kind) in [(ROOK_TRANSLATES, Rook), (BISHOP_TRANSLATES, Bishop)] {
            let mut pins = sq
                .generate_lines(&translate)
                .iter()
                .filter_map(|line| self.pinner_pinned(line, kind, color))
                .collect();
            ret.append(&mut pins);
        }
        let pins = match self.get_piece(sq).unwrap().owner {
            Player::White => &mut self.attacks.black_pins,
            Player::Black => &mut self.attacks.white_pins,
        };
        pins.clear();
        for (pinner, pinned) in ret {
            pins.insert(pinned, pinner);
        }
    }
    fn update_attacks(&mut self, m: moves::Move, captured: Option<(Piece, Square)>) {
        match m {
            moves::Move::Move(_, dst, src) => {
                self.attacks.clear_attacks_by(src, self.turn);
                self.attacks.clear_pins_by(self.turn, src);
                let mut diff = self.surrounding_pieces(src);
                diff.append(&mut self.surrounding_pieces(dst));
                diff.push((dst, self.get_piece(dst).unwrap()));
                if let Some((capt, sq)) = captured {
                    self.attacks.clear_attacks_by(sq, capt.owner);
                }
                for (sq, piece) in diff {
                    self.attacks.clear_attacks_by(sq, piece.owner);
                    self.calculate_piece_attack(sq, piece);
                }
            }
            moves::Move::Castle(king_side) => {
                let king_src = Square::new_unsafe(4, self.turn.king_rank());
                let (rook_src, rook_dst, king_dst) = if king_side {
                    (
                        Square::new_unsafe(7, self.turn.king_rank()),
                        Square::new_unsafe(5, self.turn.king_rank()),
                        Square::new_unsafe(6, self.turn.king_rank()),
                    )
                } else {
                    (
                        Square::new_unsafe(0, self.turn.king_rank()),
                        Square::new_unsafe(3, self.turn.king_rank()),
                        Square::new_unsafe(2, self.turn.king_rank()),
                    )
                };
                self.update_attacks(moves::Move::Move(false, king_dst, king_src), None);
                self.update_attacks(moves::Move::Move(false, rook_dst, rook_src), None);
            }
            moves::Move::EnPassent(src) => {
                let sq = captured.unwrap().1;
                let dst = sq.translate(0, self.turn.pawn_info().1).unwrap();
                self.update_attacks(moves::Move::Move(false, dst, src), captured);
            }
        }
    }
}

impl Board {
    /// Creates a new board from a fen string
    pub fn new(fen: &str) -> Self {
        let mut ret = fen::parse_fen(fen).unwrap();
        for i in 0..64 {
            let sq = Square::from_idx_unsafe(i);
            if let Some(p) = ret.get_piece(sq) {
                ret.calculate_piece_attack(sq, p);
            }
        }
        ret
    }
}

impl Default for Board {
    fn default() -> Self {
        Board::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    }
}
