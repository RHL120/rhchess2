use std::fmt::format;

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

fn knight(board: &Board, src: Square) -> Vec<Move> {
    [
        src.rank
            .checked_sub(2)
            .and_then(|x| Square::new(src.file + 1, x)),
        src.rank
            .checked_sub(1)
            .and_then(|x| Square::new(src.file + 2, x)),
        Square::new(src.file + 2, src.rank + 1),
        Square::new(src.file + 1, src.rank + 2),
        src.file
            .checked_sub(1)
            .and_then(|x| Square::new(x, src.rank + 2)),
        src.file
            .checked_sub(2)
            .and_then(|x| Square::new(x, src.rank + 1)),
        src.file
            .checked_sub(2)
            .and_then(|f| src.rank.checked_sub(1).and_then(|r| Square::new(f, r))),
        src.file
            .checked_sub(1)
            .and_then(|f| src.rank.checked_sub(2).and_then(|r| Square::new(f, r))),
    ]
    .iter()
    .filter_map(|&x| {
        let x = x?;
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

fn to_moves(board: &Board, src: Square, line: impl Iterator<Item = Option<Square>>) -> Vec<Move> {
    let mut ret = Vec::new();
    for i in line {
        match i {
            None => break,
            Some(s) => match board.get_piece(s) {
                None => ret.push(Move::Move(true, s, src)),
                Some(piece) => {
                    if board.turn != piece.owner {
                        ret.push(Move::Move(false, s, src));
                    }
                    break;
                }
            },
        }
    }
    ret
}

fn bishop(board: &Board, src: Square) -> Vec<Move> {
    [
        to_moves(board, src, (1..8).map(|x| src.translate(-x, -x))),
        to_moves(board, src, (1..8).map(|x| src.translate(-x, x))),
        to_moves(board, src, (1..8).map(|x| src.translate(x, -x))),
        to_moves(board, src, (1..8).map(|x| src.translate(x, x))),
    ]
    .concat()
}

fn rook(board: &Board, src: Square) -> Vec<Move> {
    [
        to_moves(board, src, (1..8).map(|x| src.translate(x, 0))),
        to_moves(board, src, (1..8).map(|x| src.translate(0, x))),
        to_moves(board, src, (1..8).map(|x| src.translate(-x, 0))),
        to_moves(board, src, (1..8).map(|x| src.translate(0, -x))),
    ]
    .concat()
}

fn queen(board: &Board, src: Square) -> Vec<Move> {
    let mut b = bishop(board, src);
    b.append(&mut rook(board, src));
    b
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
    let moves = [
        src.translate(1, 0),
        src.translate(-1, 0),
        src.translate(1, 1),
        src.translate(1, -1),
        src.translate(0, -1),
        src.translate(0, 1),
        src.translate(-1, 1),
        src.translate(-1, -1),
    ];
    let mut moves: Vec<Move> = moves
        .iter()
        .filter_map(|&sqr| {
            let sqr = sqr?;
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

pub fn get_moves(board: &Board, src: Square) -> Option<Vec<Move>> {
    match board.get_piece(src).as_ref()?.kind {
        board::PieceKind::Knight => Some(knight(board, src)),
        board::PieceKind::Bishop => Some(bishop(board, src)),
        board::PieceKind::Rook => Some(rook(board, src)),
        board::PieceKind::Queen => Some(queen(board, src)),
        board::PieceKind::Pawn => Some(pawn(board, src)),
        board::PieceKind::King => Some(king(board, src)),
    }
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
            let rank_diff = rank_diff.signum();
            let file_diff = file_diff.signum();
            [
                to_moves(
                    board,
                    src,
                    (1..8).map(|x| src.translate(file_diff * x, rank_diff * x)),
                ),
                to_moves(
                    board,
                    src,
                    (1..8).map(|x| src.translate(-file_diff * x, -rank_diff * x)),
                ),
            ]
            .concat()
        } else {
            Vec::new()
        }
    } else {
        bishop(board, src)
    }
}

fn legal_rook(board: &Board, src: Square) -> Vec<Move> {
    if let Some(pinner) = board.attacks.get_pinner(board.turn, src) {
        let rank_diff = pinner.rank as i32 - src.rank as i32;
        let file_diff = pinner.file as i32 - src.file as i32;
        //The pinner is vertical
        if rank_diff == 0 {
            let file_diff = file_diff.signum();
            [
                to_moves(board, src, (1..8).map(|x| src.translate(file_diff * x, 0))),
                to_moves(board, src, (1..8).map(|x| src.translate(-file_diff * x, 0))),
            ]
            .concat()
        } else if file_diff == 0 {
            //The pinner is horizontal
            let rank_diff = rank_diff.signum();
            [
                to_moves(board, src, (1..8).map(|x| src.translate(0, rank_diff * x))),
                to_moves(board, src, (1..8).map(|x| src.translate(0, -rank_diff * x))),
            ]
            .concat()
        } else {
            Vec::new()
        }
    } else {
        rook(board, src)
    }
}

fn legal_queen(board: &Board, src: Square) -> Vec<Move> {
    if board.attacks.get_pinner(board.turn, src).is_none() {
        queen(board, src)
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
                    let pinned = x.chunks(4).any(|chunk| {
                        use board::PieceKind::*;
                        chunk.iter().any(|&(sq, _)| sq == board.en_passant.unwrap())
                            && chunk
                                .iter()
                                .any(|(_, p)| p.kind == King && p.owner == board.turn)
                            && chunk.iter().any(|(_, p)| {
                                (p.kind == Queen || p.kind == Rook)
                                    && p.owner == board.turn.opposite()
                            })
                    });
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
    let rank_diff = (attacker.rank as i32 - attacked.rank as i32).signum();
    let file_diff = (attacker.file as i32 - attacked.file as i32).signum();
    (1..8)
        .map_while(|x| {
            let sq = attacked.translate(file_diff * x, rank_diff * x)?;
            if sq == attacker {
                None
            } else {
                Some(sq)
            }
        })
        .any(|x| x == dst)
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
            //The king can move
            if src == king_pos {
                return Some(moves);
            }
            //A piece can capture
            let &attacker = king_attacks.iter().next().unwrap();
            let moves = moves.iter().filter_map(|&mv| match mv {
                Move::Move(_, dst, _) => {
                    let apk = board.get_piece(attacker).unwrap().kind;
                    //The attacker can be captured
                    use board::PieceKind::Knight;
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
        let sq = Square::from_idx(i).unwrap();
        if let Some(p) = board.get_piece(sq) {
            if p.owner == board.turn {
                ret.append(&mut get_legal_moves(board, sq).unwrap());
            }
        }
    }
    ret
}

fn display_move(b: &board::Board, m: Move, promote: Option<board::PieceKind>) -> String {
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
        Move::Castle(_) => "castle".to_owned(),
        Move::EnPassent(src) => {
            let dst = b.en_passant.unwrap();
            format!("{}{}", src, dst)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::board::PieceKind;

    use super::*;
    #[test]
    fn move_test() {
        fn test(board: &Board, depth: u32, og_d: u32) -> usize {
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
                        board.switch_player();
                        let count = test(&board, depth - 1, og_d);
                        if depth == og_d {
                            println!("{}: {}", display_move(&board, mv, Some(i)), count);
                        }
                        ret += count
                    }
                } else {
                    let mut board = board.clone();
                    board.make_move(mv);
                    board.switch_player();
                    let count = test(&board, depth - 1, og_d);
                    if depth == og_d {
                        println!("{}: {}", display_move(&board, mv, None), count);
                    }
                    ret += count;
                }
            }
            ret
        }
        let ret = test(
            &board::Board::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"),
            2,
            2,
        );
        println!("{}", ret);
    }
}
