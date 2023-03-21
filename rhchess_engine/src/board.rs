use crate::moves;
use std::collections::HashMap;
use std::collections::HashSet;
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug)]
/// Keeps track of pinned pieces, attacked squares and en passent checks
pub struct Attacks {
    /// The key contains the squares that white attacks and the value contains
    /// the squares from which white attacks them
    pub white_attacks: HashMap<Square, HashSet<Square>>,
    /// The key contains the squares that black attacks and the value contains
    /// the squares from which black attacks them
    pub black_attacks: HashMap<Square, HashSet<Square>>,
    /// The key is a square that contains a white piece pinned by a black piece
    /// found on the value
    pub white_pins: HashMap<Square, Square>,
    /// The key is a square that contains a black piece pinned by a black piece
    /// found on the value
    pub black_pins: HashMap<Square, Square>,
    ///This bool is true if the player can take a pawn via en passant without
    ///puting their own king in check
    pub can_en_passant: bool,
}

impl Attacks {
    fn clear_attacks_by(&mut self, square: Square, player: Player) {
        let attacks = match player {
            Player::White => &mut self.white_attacks,
            Player::Black => &mut self.black_attacks,
        };
        let mut to_delete = Vec::new();
        for (key, value) in &mut *attacks {
            value.remove(&square);
            if value.len() == 0 {
                to_delete.push(*key);
            }
        }
        for i in to_delete {
            attacks.remove(&i);
        }
    }
    pub fn get_attacks_for<'a>(&'a self, p: Player, s: Square) -> Option<&'a HashSet<Square>> {
        match p {
            Player::White => self.white_attacks.get(&s),
            Player::Black => self.black_attacks.get(&s),
        }
    }
    pub fn does_attack(&self, p: Player, s: Square) -> bool {
        self.get_attacks_for(p, s).is_some()
    }
    /// Returns the square of the piece pinning the piece on `s`
    pub fn get_pinner(&self, p: Player, s: Square) -> Option<Square> {
        let pinner = match p {
            Player::White => self.black_pins.get(&s).map(|&x| x),
            Player::Black => self.white_pins.get(&s).map(|&x| x),
        };
        if let Some(pinner) = pinner {
            log::info!("The pinner is: {}", pinner);
        }
        pinner
    }
    fn clear_pins_by(&mut self, p: Player, s: Square) {
        log::info!("Clearing pins by {:#?} {}", p, s);
        let pins = match p {
            Player::White => &mut self.white_pins,
            Player::Black => &mut self.black_pins,
        };
        let pos = pins
            .iter()
            .find_map(|(&k, &v)| if s == v { Some(k) } else { None });
        if let Some(pos) = pos {
            log::info!("cleared pin: {:#?}, {}", p, s);
            pins.remove(&pos);
        }
    }
}

impl Default for Attacks {
    fn default() -> Self {
        Self {
            white_attacks: HashMap::new(),
            black_attacks: HashMap::new(),
            white_pins: HashMap::new(),
            black_pins: HashMap::new(),
            can_en_passant: true,
        }
    }
}

/// The board representation
#[derive(Debug)]
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
    pub fn current_king(&self) -> Square {
        match self.turn {
            Player::White => self.white_pos,
            Player::Black => self.black_pos,
        }
    }
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
                self.positions[(src.rank * 8 + src.file) as usize] = None;
                let capt = self.positions[(dst.rank * 8 + dst.file) as usize];
                self.positions[(dst.rank * 8 + dst.file) as usize] = Some(piece);
                self.update_attacks(m, capt.map(|x| (x, dst)))
            }
            moves::Move::EnPassent(src) => {
                let piece = self.positions[(src.rank * 8 + src.file) as usize].unwrap();
                self.positions[(src.rank * 8 + src.file) as usize] = None;
                let en_passant = self.en_passant.unwrap();
                let dst = en_passant.translate(0, piece.owner.pawn_info().1).unwrap();
                let capt = self.positions[(en_passant.rank * 8 + en_passant.file) as usize];
                self.positions[(dst.rank * 8 + dst.file) as usize] = Some(piece);
                self.positions[(en_passant.rank * 8 + en_passant.file) as usize] = None;
                self.update_attacks(m, capt.map(|x| (x, en_passant)));
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
                self.update_attacks(m, None);
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
                let p = Piece {
                    kind,
                    owner: self.turn,
                };
                self.positions[(8 * dst.rank + dst.file) as usize] = Some(p);
                self.calculate_piece_attack(dst, p);
            }
            _ => unreachable!(),
        }
    }
    fn first_piece_is(
        &self,
        rng: impl Iterator<Item = Square>,
        kinds: &[PieceKind],
    ) -> Option<(Piece, Square)> {
        for i in rng {
            if let Some(piece) = self.get_piece(i) {
                if kinds.contains(&piece.kind) {
                    return Some((piece, i));
                } else {
                    return None;
                }
            }
        }
        None
    }
    fn surrounding_pieces(&mut self, sq: Square) -> Vec<(Piece, Square)> {
        use PieceKind::{Bishop, Queen, Rook};
        let mut ret: Vec<(Piece, Square)> = [
            self.first_piece_is((1..8).map_while(|x| sq.translate(x, 0)), &[Rook, Queen]),
            self.first_piece_is((1..8).map_while(|x| sq.translate(0, x)), &[Rook, Queen]),
            self.first_piece_is((1..8).map_while(|x| sq.translate(-x, 0)), &[Rook, Queen]),
            self.first_piece_is((1..8).map_while(|x| sq.translate(0, -x)), &[Rook, Queen]),
        ]
        .iter()
        .filter_map(|&x| x)
        .collect();
        let mut bishop = [
            self.first_piece_is((1..8).map_while(|x| sq.translate(-x, -x)), &[Bishop, Queen]),
            self.first_piece_is((1..8).map_while(|x| sq.translate(-x, x)), &[Bishop, Queen]),
            self.first_piece_is((1..8).map_while(|x| sq.translate(x, -x)), &[Bishop, Queen]),
            self.first_piece_is((1..8).map_while(|x| sq.translate(x, x)), &[Bishop, Queen]),
        ]
        .iter()
        .filter_map(|&x| x)
        .collect();
        let mut knight = [
            sq.rank
                .checked_sub(2)
                .and_then(|x| Square::new(sq.file + 1, x)),
            sq.rank
                .checked_sub(1)
                .and_then(|x| Square::new(sq.file + 2, x)),
            Square::new(sq.file + 2, sq.rank + 1),
            Square::new(sq.file + 1, sq.rank + 2),
            sq.file
                .checked_sub(1)
                .and_then(|x| Square::new(x, sq.rank + 2)),
            sq.file
                .checked_sub(2)
                .and_then(|x| Square::new(x, sq.rank + 1)),
            sq.file
                .checked_sub(2)
                .and_then(|f| sq.rank.checked_sub(1).and_then(|r| Square::new(f, r))),
            sq.file
                .checked_sub(1)
                .and_then(|f| sq.rank.checked_sub(2).and_then(|r| Square::new(f, r))),
        ]
        .iter()
        .filter_map(|&sq| {
            let sq = sq?;
            self.get_piece(sq).and_then(|p| {
                if p.kind == PieceKind::Knight {
                    Some((p, sq))
                } else {
                    None
                }
            })
        })
        .collect();
        let mut king = [
            sq.translate(1, 0),
            sq.translate(-1, 0),
            sq.translate(1, 1),
            sq.translate(1, -1),
            sq.translate(0, -1),
            sq.translate(0, 1),
            sq.translate(-1, 1),
            sq.translate(-1, -1),
        ]
        .iter()
        .filter_map(|&sq| {
            let sq = sq?;
            self.get_piece(sq).and_then(|p| {
                if p.kind == PieceKind::King {
                    Some((p, sq))
                } else {
                    None
                }
            })
        })
        .collect();
        ret.append(&mut bishop);
        ret.append(&mut knight);
        ret.append(&mut king);
        ret
    }
    fn update_knight_attacks(&mut self, square: Square, p: Player) {
        let r = match p {
            Player::White => &mut self.attacks.white_attacks,
            Player::Black => &mut self.attacks.black_attacks,
        };
        let attacks: Vec<Square> = [
            square
                .rank
                .checked_sub(2)
                .and_then(|x| Square::new(square.file + 1, x)),
            square
                .rank
                .checked_sub(1)
                .and_then(|x| Square::new(square.file + 2, x)),
            Square::new(square.file + 2, square.rank + 1),
            Square::new(square.file + 1, square.rank + 2),
            square
                .file
                .checked_sub(1)
                .and_then(|x| Square::new(x, square.rank + 2)),
            square
                .file
                .checked_sub(2)
                .and_then(|x| Square::new(x, square.rank + 1)),
            square
                .file
                .checked_sub(2)
                .and_then(|f| square.rank.checked_sub(1).and_then(|r| Square::new(f, r))),
            square
                .file
                .checked_sub(1)
                .and_then(|f| square.rank.checked_sub(2).and_then(|r| Square::new(f, r))),
        ]
        .iter()
        .filter_map(|&x| x)
        .collect();
        for i in attacks {
            if !r.contains_key(&i) {
                r.insert(i, HashSet::new());
            }
            r.get_mut(&i).unwrap().insert(square);
        }
    }
    fn update_king_attacks(&mut self, square: Square, p: Player) {
        let r = match p {
            Player::White => &mut self.attacks.white_attacks,
            Player::Black => &mut self.attacks.black_attacks,
        };
        let attacks: Vec<Square> = [
            square.translate(1, 0),
            square.translate(-1, 0),
            square.translate(1, 1),
            square.translate(1, -1),
            square.translate(0, -1),
            square.translate(0, 1),
            square.translate(-1, 1),
            square.translate(-1, -1),
        ]
        .iter()
        .filter_map(|&x| x)
        .collect();
        for i in attacks {
            if !r.contains_key(&i) {
                r.insert(i, HashSet::new());
            }
            r.get_mut(&i).unwrap().insert(square);
        }
        self.on_king_update_pins(square)
    }
    fn to_attack(&self, line: impl Iterator<Item = Option<Square>>) -> HashSet<Square> {
        let mut ret = HashSet::new();
        for i in line {
            match i {
                None => break,
                Some(s) => match self.get_piece(s) {
                    None => ret.insert(s),
                    Some(_) => {
                        ret.insert(s);
                        break;
                    }
                },
            };
        }
        ret
    }
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
    fn update_bishop_attacks(&mut self, square: Square, p: Player) {
        let attacks = [
            self.to_attack((1..8).map(|x| square.translate(-x, -x))),
            self.to_attack((1..8).map(|x| square.translate(-x, x))),
            self.to_attack((1..8).map(|x| square.translate(x, -x))),
            self.to_attack((1..8).map(|x| square.translate(x, x))),
        ];
        log::info!("updating {}", square);
        self.attacks.clear_pins_by(p, square);
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
                if !r.contains_key(&j) {
                    r.insert(j, HashSet::new());
                }
                r.get_mut(&j).unwrap().insert(square);
            }
        }
    }
    fn update_rook_attacks(&mut self, square: Square, p: Player) {
        let attacks = [
            self.to_attack((1..8).map(|x| square.translate(x, 0))),
            self.to_attack((1..8).map(|x| square.translate(0, x))),
            self.to_attack((1..8).map(|x| square.translate(-x, 0))),
            self.to_attack((1..8).map(|x| square.translate(0, -x))),
        ];
        if let Some(pin) = self.get_pin(square, |x, y| x == 0 || y == 0, p) {
            match p {
                Player::White => self.attacks.white_pins.insert(square, pin),
                Player::Black => self.attacks.black_pins.insert(square, pin),
            };
        }
        let r = match p {
            Player::White => &mut self.attacks.white_attacks,
            Player::Black => &mut self.attacks.black_attacks,
        };
        for i in attacks {
            for j in i {
                if !r.contains_key(&j) {
                    r.insert(j, HashSet::new());
                }
                r.get_mut(&j).unwrap().insert(square);
            }
        }
    }
    fn update_pawn_attacks(&mut self, square: Square, p: Player) {
        let r = match p {
            Player::White => &mut self.attacks.white_attacks,
            Player::Black => &mut self.attacks.black_attacks,
        };
        let (_, rank_multiple) = p.pawn_info();
        for i in [-1, 1] {
            if let Some(i) = square.translate(i, rank_multiple) {
                if !r.contains_key(&i) {
                    r.insert(i, HashSet::new());
                }
                r.get_mut(&i).unwrap().insert(square);
            }
        }
    }
    fn update_queen_attacks(&mut self, square: Square, p: Player) {
        self.update_rook_attacks(square, p);
        self.update_bishop_attacks(square, p);
    }
    fn calculate_piece_attack(&mut self, square: Square, p: Piece) {
        let pl = p.owner;
        match p.kind {
            PieceKind::Rook => self.update_rook_attacks(square, pl),
            PieceKind::Queen => self.update_queen_attacks(square, pl),
            PieceKind::Bishop => self.update_bishop_attacks(square, pl),
            PieceKind::Knight => self.update_knight_attacks(square, pl),
            PieceKind::King => self.update_king_attacks(square, pl),
            PieceKind::Pawn => self.update_pawn_attacks(square, pl),
        };
    }
    fn pinner_pinned(
        &self,
        rng: impl Iterator<Item = Square>,
        kinds: &[PieceKind],
    ) -> Option<(Square, Square)> {
        let mut pinner = None;
        let mut pinned = None;
        for i in rng {
            if let Some(piece) = self.get_piece(i) {
                if piece.owner == self.turn {
                    if pinned.is_some() {
                        break;
                    }
                    pinned = Some(i);
                } else {
                    pinned?;
                    if kinds.contains(&piece.kind) {
                        pinner = Some(i);
                    }
                    break;
                }
            }
        }
        Some((pinner?, pinned?))
    }
    fn on_king_update_pins(&mut self, sq: Square) {
        use PieceKind::{Bishop, Queen, Rook};
        let mut ret: Vec<(Square, Square)> = [
            self.pinner_pinned((1..8).map_while(|x| sq.translate(x, 0)), &[Rook, Queen]),
            self.pinner_pinned((1..8).map_while(|x| sq.translate(0, x)), &[Rook, Queen]),
            self.pinner_pinned((1..8).map_while(|x| sq.translate(-x, 0)), &[Rook, Queen]),
            self.pinner_pinned((1..8).map_while(|x| sq.translate(0, -x)), &[Rook, Queen]),
        ]
        .iter()
        .filter_map(|&x| x)
        .collect();
        let mut bishop: Vec<(Square, Square)> = [
            self.pinner_pinned((1..8).map_while(|x| sq.translate(-x, -x)), &[Bishop, Queen]),
            self.pinner_pinned((1..8).map_while(|x| sq.translate(-x, x)), &[Bishop, Queen]),
            self.pinner_pinned((1..8).map_while(|x| sq.translate(x, -x)), &[Bishop, Queen]),
            self.pinner_pinned((1..8).map_while(|x| sq.translate(x, x)), &[Bishop, Queen]),
        ]
        .iter()
        .filter_map(|&x| x)
        .collect();
        ret.append(&mut bishop);
        let pins = match self.turn {
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
                let mut diff = self.surrounding_pieces(src);
                diff.append(&mut self.surrounding_pieces(dst));
                log::info!("Got dst: {}", dst);
                diff.push((self.get_piece(dst).unwrap(), dst));
                if let Some((capt, sq)) = captured {
                    log::info!("piece capt: {} {:#?}", sq, capt.owner);
                    self.attacks.clear_attacks_by(sq, capt.owner);
                }
                for (piece, sq) in diff {
                    log::info!("{} surrounds {}", sq, dst);
                    self.attacks.clear_attacks_by(sq, piece.owner);
                    self.calculate_piece_attack(sq, piece);
                }
            }
            moves::Move::Castle(king_side) => {
                let king_src = Square::new(4, self.turn.king_rank()).unwrap();
                let (rook_src, rook_dst, king_dst) = if king_side {
                    (
                        Square::new(0, self.turn.king_rank()).unwrap(),
                        Square::new(3, self.turn.king_rank()).unwrap(),
                        Square::new(2, self.turn.king_rank()).unwrap(),
                    )
                } else {
                    (
                        Square::new(7, self.turn.king_rank()).unwrap(),
                        Square::new(5, self.turn.king_rank()).unwrap(),
                        Square::new(6, self.turn.king_rank()).unwrap(),
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
        log::info!("{:#?}", self.attacks);
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
        let mut ret = Self {
            positions,
            turn: White,
            castling_rights: CastlingRights::default(),
            en_passant: None,
            reversible_moves: 0,
            full_moves: 1,
            attacks: Attacks::default(),
            white_pos: Square { rank: 0, file: 4 },
            black_pos: Square { rank: 7, file: 4 },
        };
        for i in 0..64 {
            let sq = Square::from_idx(i).unwrap();
            if let Some(p) = ret.get_piece(sq) {
                ret.calculate_piece_attack(sq, p);
            }
        }
        ret
    }
}
