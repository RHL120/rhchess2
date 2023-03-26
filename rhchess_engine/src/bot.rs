use std::cmp;

use crate::board;
use crate::moves;
use crate::moves::get_all_legal_moves;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum GameStage {
    MiddleGame,
    EndGame,
}

const PAWN_VALUE_MIDDLE_GAME: i32 = 95; //cps
const KNIGHT_VALUE_MIDDLE_GAME: i32 = 310; //cps
const BISHOP_VALUE_MIDDLE_GAME: i32 = 320; //cps
const ROOK_VALUE_MIDDLE_GAME: i32 = 460; //cps
const QUEEN_VALUE_MIDDLE_GAME: i32 = 900; //cps

const PAWN_VALUE_END_GAME: i32 = 105; //cps
const KNIGHT_VALUE_END_GAME: i32 = 300; //cps
const BISHOP_VALUE_END_GAME: i32 = 330; //cps
const ROOK_VALUE_END_GAME: i32 = 490; //cps
const QUEEN_VALUE_END_GAME: i32 = 950; //cps
fn piece_value(
    board: &board::Board,
    square: board::Square,
    kind: board::PieceKind,
    stage: GameStage,
) -> i32 {
    use board::PieceKind::*;
    match kind {
        Pawn => match stage {
            GameStage::MiddleGame => PAWN_VALUE_MIDDLE_GAME,
            GameStage::EndGame => PAWN_VALUE_END_GAME,
        },
        Knight => match stage {
            GameStage::MiddleGame => KNIGHT_VALUE_MIDDLE_GAME,
            GameStage::EndGame => KNIGHT_VALUE_END_GAME,
        },
        Bishop => match stage {
            GameStage::MiddleGame => BISHOP_VALUE_MIDDLE_GAME,
            GameStage::EndGame => BISHOP_VALUE_END_GAME,
        },
        Rook => match stage {
            GameStage::MiddleGame => ROOK_VALUE_MIDDLE_GAME,
            GameStage::EndGame => ROOK_VALUE_END_GAME,
        },
        Queen => match stage {
            GameStage::MiddleGame => QUEEN_VALUE_MIDDLE_GAME,
            GameStage::EndGame => QUEEN_VALUE_END_GAME,
        },
        King => match stage {
            GameStage::MiddleGame => 0,
            GameStage::EndGame => 0,
        },
    }
}

fn evaluate(board: &board::Board) -> i32 {
    let (wv, bv) = board
        .positions
        .iter()
        .enumerate()
        .filter_map(|(i, &p)| Some((i, p?)))
        .fold((0, 0), |(wc, bc), (idx, piece)| match piece.owner {
            board::Player::Black => (
                wc,
                bc + piece_value(
                    board,
                    board::Square::from_idx_unsafe(idx as u8),
                    piece.kind,
                    GameStage::MiddleGame,
                ),
            ),
            board::Player::White => (
                wc + piece_value(
                    board,
                    board::Square::from_idx_unsafe(idx as u8),
                    piece.kind,
                    GameStage::MiddleGame,
                ),
                bc,
            ),
        });
    match board.turn {
        board::Player::White => wv - bv,
        board::Player::Black => bv - wv,
    }
}

fn search(board: &board::Board) -> Option<moves::Move> {
    fn maxi(board: &board::Board, depth: usize) -> i32 {
        let mut max = i32::MIN;
        if depth == 0 {
            return evaluate(board);
        }
        for i in moves::get_all_legal_moves(board) {
            let mut board = board.clone();
            board.make_move(i);
            board.switch_player();
            let score = mini(&board, depth - 1);
            if score > max {
                max = score;
            }
        }
        max
    }
    fn mini(board: &board::Board, depth: usize) -> i32 {
        let mut min = i32::MIN;
        if depth == 0 {
            return -evaluate(board);
        }
        for i in moves::get_all_legal_moves(board) {
            let mut board = board.clone();
            board.make_move(i);
            board.switch_player();
            let res = maxi(&board, depth - 1);
            if res < min {
                min = res;
            }
        }
        min
    }
    moves::get_all_legal_moves(board)
        .iter()
        .map(|x| {
            let mut board = board.clone();
            board.make_move(*x);
            board.switch_player();
            (x, mini(&board, 3))
        })
        .min_by(|x, y| x.1.cmp(&y.1))
        .map(|x| x.0)
        .copied()
}

pub fn make_move(board: &mut board::Board) {
    board.make_move(search(board).unwrap());
}
