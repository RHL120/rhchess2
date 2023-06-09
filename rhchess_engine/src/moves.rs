use crate::board;
use crate::board::Board;
use crate::board::Square;
#[derive(Clone, Copy, Debug)]
pub enum Move {
    /// Castle(king_side)
    Castle(bool),
    /// EnPassent(src)
    EnPassent(Square),
    /// Move(rev, dst, src)
    Move(bool, Square, Square),
}

pub const BISHOP_TRANSLATES: [(i32, i32); 4] = [(-1, 1), (1, -1), (-1, -1), (1, 1)];
pub const ROOK_TRANSLATES: [(i32, i32); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];
pub const KNIGHT_TRANSLATES: [(i32, i32); 8] = [
    (1, 2),
    (-1, 2),
    (-2, 1),
    (-2, -1),
    (-1, -2),
    (1, -2),
    (2, -1),
    (2, 1),
];
pub const QUEEN_TRANSLATES: [(i32, i32); 8] = [
    (1, 0),
    (0, 1),
    (-1, 0),
    (0, -1),
    (-1, 1),
    (1, -1),
    (-1, -1),
    (1, 1),
];
pub const KING_TRANSLATES: [(i32, i32); 8] = [
    (1, 0),
    (-1, 0),
    (1, 1),
    (1, -1),
    (0, -1),
    (0, 1),
    (-1, 1),
    (-1, -1),
];

fn knight(board: &Board, src: Square) -> Vec<Move> {
    KNIGHT_TRANSLATES
        .iter()
        .filter_map(|&(fd, rd)| {
            let x = src.translate(fd, rd)?;
            match board.get_piece(x) {
                Some(piece) => {
                    if piece.owner != board.turn {
                        Some(Move::Move(false, x, src))
                    } else {
                        None
                    }
                }
                None => Some(Move::Move(true, x, src)),
            }
        })
        .collect()
}

fn sliders(board: &Board, src: Square, diffs: &[(i32, i32)]) -> Vec<Move> {
    let mut ret = Vec::new();
    for line in src.generate_lines(diffs) {
        for square in line {
            match board.get_piece(square) {
                None => ret.push(Move::Move(true, square, src)),
                Some(piece) => {
                    if board.turn != piece.owner {
                        ret.push(Move::Move(false, square, src));
                    }
                    break;
                }
            }
        }
    }
    ret
}

fn pawn(board: &Board, src: Square) -> Vec<Move> {
    let (init_rank, rank_multiple) = board.turn.pawn_info();
    let to_move = |&sqr| {
        let sqr = sqr?;
        if board.get_piece(sqr).is_none() {
            Some(Move::Move(false, sqr, src))
        } else {
            None
        }
    };
    let mut moves: Vec<Move> = if src.rank == init_rank {
        [
            src.translate(0, rank_multiple),
            src.translate(0, 2 * rank_multiple),
        ]
        .iter()
        .map_while(to_move)
        .collect()
    } else {
        //Nope
        [src.translate(0, rank_multiple)]
            .iter()
            .map_while(to_move)
            .collect()
    };
    let mut captures = [-1, 1]
        .iter()
        .filter_map(|&dir| {
            let sqr = src.translate(dir, rank_multiple)?;
            match board.get_piece(sqr) {
                Some(piece) => {
                    if piece.owner != board.turn {
                        Some(Move::Move(false, sqr, src))
                    } else {
                        None
                    }
                }
                None => None,
            }
        })
        .collect();
    if let Some(en_passant) = board.en_passant {
        if let Some(true) = en_passant.translate(1, 0).map(|x| x == src) {
            moves.push(Move::EnPassent(src));
        } else if let Some(true) = en_passant.translate(-1, 0).map(|x| x == src) {
            moves.push(Move::EnPassent(src));
        }
    }
    moves.append(&mut captures);
    moves
}

fn king(board: &Board, src: Square) -> Vec<Move> {
    let mut moves: Vec<Move> = KING_TRANSLATES
        .iter()
        .filter_map(|&(fd, rd)| {
            let sqr = src.translate(fd, rd)?;
            let piece = board.get_piece(sqr);
            match piece {
                Some(piece) => {
                    if piece.owner == board.turn {
                        None
                    } else {
                        Some(Move::Move(false, sqr, src))
                    }
                }
                None => Some(Move::Move(true, sqr, src)),
            }
        })
        .collect();
    let (queen_side, king_side) = match board.turn {
        board::Player::White => (
            board.castling_rights.white_queen,
            board.castling_rights.white_king,
        ),
        board::Player::Black => (
            board.castling_rights.black_queen,
            board.castling_rights.black_king,
        ),
    };
    if king_side
        && board.get_piece(src.translate(1, 0).unwrap()).is_none()
        && board.get_piece(src.translate(2, 0).unwrap()).is_none()
    {
        moves.push(Move::Castle(true))
    }
    if queen_side
        && board.get_piece(src.translate(-1, 0).unwrap()).is_none()
        && board.get_piece(src.translate(-2, 0).unwrap()).is_none()
        && board.get_piece(src.translate(-3, 0).unwrap()).is_none()
    {
        moves.push(Move::Castle(false))
    }
    moves
}

pub fn legal_king(board: &Board, src: Square) -> Vec<Move> {
    king(board, src)
        .iter()
        .filter_map(|&mv| match mv {
            Move::Move(_, dst, _) => {
                if !board.attacks.does_attack(board.turn.opposite(), dst) {
                    Some(mv)
                } else {
                    None
                }
            }
            Move::Castle(king_side) => {
                let king_rank = board.turn.king_rank();
                if board
                    .attacks
                    .does_attack(board.turn.opposite(), board.current_king())
                {
                    return None;
                }
                if king_side {
                    let first = Square {
                        rank: king_rank,
                        file: 5,
                    };
                    let second = Square {
                        rank: king_rank,
                        file: 6,
                    };
                    let opp = board.turn.opposite();
                    if board.attacks.does_attack(opp, first)
                        || board.attacks.does_attack(opp, second)
                    {
                        None
                    } else {
                        Some(mv)
                    }
                } else {
                    let first = Square {
                        rank: king_rank,
                        file: 2,
                    };
                    let second = Square {
                        rank: king_rank,
                        file: 3,
                    };
                    let opp = board.turn.opposite();
                    if board.attacks.does_attack(opp, first)
                        || board.attacks.does_attack(opp, second)
                    {
                        None
                    } else {
                        Some(mv)
                    }
                }
            }
            Move::EnPassent(_) => unreachable!(),
        })
        .collect()
}

fn legal_knight(board: &Board, src: Square) -> Vec<Move> {
    if board.attacks.get_pinner(board.turn, src).is_none() {
        knight(board, src)
    } else {
        vec![]
    }
}

fn legal_bishop(board: &Board, src: Square) -> Vec<Move> {
    if let Some(pinner) = board.attacks.get_pinner(board.turn, src) {
        let rank_diff = pinner.rank as i32 - src.rank as i32;
        let file_diff = pinner.file as i32 - src.file as i32;
        //The pinner is on a diagonal
        if rank_diff.abs() == file_diff.abs() {
            let rd = rank_diff.signum();
            let fd = file_diff.signum();
            sliders(board, src, &[(rd, fd), (-rd, -fd)])
        } else {
            Vec::new()
        }
    } else {
        sliders(board, src, &BISHOP_TRANSLATES)
    }
}

fn legal_rook(board: &Board, src: Square) -> Vec<Move> {
    if let Some(pinner) = board.attacks.get_pinner(board.turn, src) {
        let rank_diff = pinner.rank as i32 - src.rank as i32;
        let file_diff = pinner.file as i32 - src.file as i32;
        //The pinner is vertical
        if rank_diff == 0 {
            let file_diff = file_diff.signum();
            sliders(board, src, &[(file_diff, 0), (-file_diff, 0)])
        } else if file_diff == 0 {
            //The pinner is horizontal
            let rank_diff = rank_diff.signum();
            sliders(board, src, &[(0, rank_diff), (0, -rank_diff)])
        } else {
            Vec::new()
        }
    } else {
        sliders(board, src, &ROOK_TRANSLATES)
    }
}

fn legal_queen(board: &Board, src: Square) -> Vec<Move> {
    if board.attacks.get_pinner(board.turn, src).is_none() {
        sliders(board, src, &QUEEN_TRANSLATES)
    } else {
        let rook = legal_rook(board, src);
        if rook.is_empty() {
            legal_bishop(board, src)
        } else {
            rook
        }
    }
}

fn legal_pawn(board: &Board, src: Square) -> Vec<Move> {
    if let Some(pinner) = board.attacks.get_pinner(board.turn, src) {
        let (_, rank_multiple) = board.turn.pawn_info();
        let pin_check = |dst| match dst == pinner {
            true => Some(dst),
            false => None,
        };
        src.translate(-1, rank_multiple)
            .and_then(pin_check)
            .or_else(|| src.translate(1, rank_multiple).and_then(pin_check))
            .map(|x| vec![Move::Move(false, x, src)])
            .unwrap_or(vec![])
    } else {
        let mvs = pawn(board, src)
            .iter()
            .filter_map(|&x| match x {
                r @ Move::EnPassent(_) => {
                    let x = (0..8)
                        .filter_map(|x| {
                            let sq = Square::new(x, board.en_passant.unwrap().rank)?;
                            Some((sq, board.get_piece(sq)?))
                        })
                        .collect::<Vec<(board::Square, board::Piece)>>();
                    use board::PieceKind::*;
                    let mut pinned = false;
                    for i in 0..x.len() {
                        let sub = match x.get(i..i + 4) {
                            Some(x) => x,
                            None => break,
                        };
                        pinned = sub.iter().any(|&(sq, _)| sq == board.en_passant.unwrap())
                            && sub.iter().any(|&(sq, _)| sq == src)
                            && sub
                                .iter()
                                .any(|&(_, p)| p.kind == King && p.owner == board.turn)
                            && sub.iter().any(|&(_, p)| {
                                p.owner != board.turn && (p.kind == Rook || p.kind == Queen)
                            });
                        if pinned {
                            break;
                        }
                    }
                    if !pinned {
                        Some(r)
                    } else {
                        None
                    }
                }
                r => Some(r),
            })
            .collect();
        mvs
    }
}

fn legal_moves(board: &Board, src: Square) -> Option<Vec<Move>> {
    Some(match board.get_piece(src).as_ref()?.kind {
        board::PieceKind::Knight => legal_knight(board, src),
        board::PieceKind::Bishop => legal_bishop(board, src),
        board::PieceKind::Rook => legal_rook(board, src),
        board::PieceKind::Queen => legal_queen(board, src),
        board::PieceKind::King => legal_king(board, src),
        board::PieceKind::Pawn => legal_pawn(board, src),
    })
}

fn blocks(dst: Square, attacker: Square, attacked: Square) -> bool {
    attacked.between(attacker).iter().any(|&x| x == dst)
}

pub fn get_legal_moves(board: &Board, src: Square) -> Option<Vec<Move>> {
    let king_pos = board.current_king();
    let king_attacks = board
        .attacks
        .get_attacks_for(board.turn.opposite(), king_pos);
    // the king must get out of a check
    if let Some(king_attacks) = king_attacks {
        if king_attacks.len() >= 2 {
            //There is a double check, the king must move
            if src == king_pos {
                return Some(legal_king(board, src));
            } else {
                return Some(Vec::new());
            }
        } else {
            let moves = legal_moves(board, src)?;
            let &attacker = king_attacks.iter().next().unwrap();
            let attack_piece = board.get_piece(attacker).unwrap();
            use board::PieceKind::*;
            //The king can move
            if src == king_pos {
                //If the attacker is a sliding piece, the king can't stay
                //on the line attacked by the piece
                if [Rook, Queen, Bishop].contains(&attack_piece.kind) {
                    let rank_diff = (king_pos.rank as i32 - attacker.rank as i32).signum();
                    let file_diff = (king_pos.file as i32 - attacker.file as i32).signum();
                    let moves = moves
                        .iter()
                        .filter(|mv| {
                            let dst = match mv {
                                Move::Move(_, dst, _) => dst,
                                _ => unreachable!(),
                            };
                            !(1..8)
                                .filter_map(|x| attacker.translate(file_diff * x, rank_diff * x))
                                .any(|x| x == *dst)
                        })
                        .copied()
                        .collect();
                    return Some(moves);
                }
                return Some(moves);
            }
            //A piece can capture
            let moves = moves.iter().filter_map(|&mv| match mv {
                Move::Move(_, dst, _) => {
                    let apk = board.get_piece(attacker).unwrap().kind;
                    //The attacker can be captured
                    if dst == attacker || (apk != Knight && blocks(dst, attacker, king_pos)) {
                        Some(mv)
                    } else {
                        None
                    }
                }
                Move::EnPassent(_) => {
                    //The attacker can be captured
                    if board.en_passant.unwrap() == attacker {
                        Some(mv)
                    } else {
                        None
                    }
                }
                Move::Castle(_) => unreachable!(),
            });
            return Some(moves.collect());
        }
    }
    legal_moves(board, src)
}
pub fn get_all_legal_moves(board: &Board) -> Vec<Move> {
    let mut ret = Vec::new();
    for i in 0..64 {
        let sq = Square::from_idx_unsafe(i);
        if let Some(p) = board.get_piece(sq) {
            if p.owner == board.turn {
                ret.append(&mut get_legal_moves(board, sq).unwrap());
            }
        }
    }
    ret
}

#[cfg(test)]
mod tests {
    use crate::board::PieceKind;

    fn display_move(
        b: &board::Board,
        m: Move,
        promote: Option<board::PieceKind>,
        t: board::Player,
    ) -> String {
        match m {
            Move::Move(_, dst, src) => {
                let r = format!("{}{}", src, dst);
                if let Some(promote) = promote {
                    let piece = match promote {
                        board::PieceKind::Pawn => 'p',
                        board::PieceKind::Rook => 'r',
                        board::PieceKind::Queen => 'q',
                        board::PieceKind::Knight => 'n',
                        board::PieceKind::King => 'n',
                        board::PieceKind::Bishop => 'b',
                    };
                    format!("{}{}", r, piece)
                } else {
                    r
                }
            }
            Move::Castle(king_side) => {
                let rank = t.king_rank();
                let king = Square { rank, file: 4 };
                if king_side {
                    format!("{}{}", king, board::Square { rank, file: 6 },)
                } else {
                    let rank = t.king_rank();
                    let king = Square { rank, file: 4 };
                    format!("{}{}", king, board::Square { rank, file: 2 })
                }
            }
            Move::EnPassent(src) => {
                let dst = b.en_passant.unwrap();
                format!("{}{}", src, dst)
            }
        }
    }

    use super::*;
    fn perft(board: &Board, depth: u32, og_d: u32) -> usize {
        if depth == 0 {
            return 1;
        }
        let mut ret = 0;
        for mv in get_all_legal_moves(board) {
            if board.is_promotion(mv) {
                for i in [
                    PieceKind::Queen,
                    PieceKind::Rook,
                    PieceKind::Bishop,
                    PieceKind::Knight,
                ] {
                    let mut board = board.clone();
                    board.make_move(mv);
                    board.make_promotion(mv, i);
                    let p = board.turn;
                    board.switch_player();
                    let count = perft(&board, depth - 1, og_d);
                    if depth == og_d {
                        println!("{}: {}", display_move(&board, mv, Some(i), p), count);
                    }
                    ret += count
                }
            } else {
                let mut board = board.clone();
                board.make_move(mv);
                let player = board.turn;
                board.switch_player();
                let count = perft(&board, depth - 1, og_d);
                if depth == og_d {
                    println!("{}: {}", display_move(&board, mv, None, player), count);
                }
                ret += count;
            }
        }
        ret
    }
    #[test]
    fn initial_postion() {
        let ret = perft(&board::Board::default(), 3, 3);
        assert_eq!(ret, 8902);
    }
    #[test]
    fn position2() {
        let ret = perft(
            &board::Board::new(
                "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            ),
            3,
            3,
        );
        assert_eq!(ret, 97862);
    }
    #[test]
    fn position3() {
        let ret = perft(
            &board::Board::new("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"),
            4,
            4,
        );
        assert_eq!(ret, 43238);
    }
    #[test]
    fn position5() {
        let ret = perft(
            &board::Board::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"),
            3,
            3,
        );
        assert_eq!(ret, 62379);
    }
    #[test]
    fn position6() {
        let ret = perft(
            &board::Board::new(
                "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
            ),
            3,
            3,
        );
        assert_eq!(ret, 89890);
    }
}
