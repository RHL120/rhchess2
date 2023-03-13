use crate::board;
use crate::board::Board;
use crate::board::Square;

#[derive(Clone, Copy, Debug)]
pub enum Move {
    /// Castle(king_size)
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

pub fn get_move(board: &Board, src: Square) -> Option<Vec<Move>> {
    match board.get_piece(src).as_ref()?.kind {
        board::PieceKind::Knight => Some(knight(board, src)),
        board::PieceKind::Bishop => Some(bishop(board, src)),
        board::PieceKind::Rook => Some(rook(board, src)),
        board::PieceKind::Queen => Some(queen(board, src)),
        _ => None,
    }
}
