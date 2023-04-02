use crate::board;
use crate::moves;
use crate::moves::get_legal_moves;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
#[allow(dead_code)]
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

const PAWN_PSQT_MIDDLE_GAME: [i32; 64] = [
    0, 0, 0, 0, 0, 0, 0, 0, 5, 7, 10, 13, 13, 10, 7, 5, 7, 10, 13, 20, 20, 13, 10, 7, 13, 17, 30,
    45, 45, 30, 17, 13, 17, 25, 33, 50, 50, 33, 25, 17, 25, 35, 45, 55, 55, 45, 35, 25, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];

const KNIGHT_PSQT_MIDDLE_GAME: [i32; 64] = [
    -30, -20, -13, -7, -7, -13, -20, -30, -13, -7, 0, 7, 7, 0, -7, -13, -7, 7, 20, 27, 27, 20, 7,
    -7, 7, 20, 35, 40, 40, 35, 20, 7, 20, 30, 40, 50, 50, 40, 30, 20, 27, 55, 68, 80, 80, 68, 55,
    27, 20, 30, 40, 50, 50, 40, 30, 20, 10, 20, 30, 40, 40, 30, 20, 10,
];

const BISHOP_PSQT_MIDDLE_GAME: [i32; 64] = [
    -13, -7, 0, 7, 7, 0, -7, -13, -7, 0, 7, 13, 13, 7, 0, -7, 0, 13, 20, 25, 25, 20, 13, 0, 7, 20,
    25, 40, 40, 25, 20, 7, 20, 30, 40, 50, 50, 40, 30, 20, 24, 28, 45, 55, 55, 45, 28, 25, 0, 20,
    25, 30, 30, 25, 20, 0, -10, -5, 0, 5, 5, 0, -5, -10,
];

const ROOK_PSQT_MIDDLE_GAME: [i32; 64] = [
    -20, -10, 10, 20, 20, 10, -10, -20, -15, -5, 15, 25, 25, 15, -5, -15, -10, 0, 20, 30, 30, 20,
    0, -10, -5, 5, 25, 35, 35, 25, 5, -5, 0, 10, 30, 40, 40, 30, 10, 0, 5, 15, 35, 45, 45, 45, 15,
    5, 24, 35, 40, 50, 50, 40, 35, 25, 5, 15, 35, 45, 45, 45, 15, 5,
];

const QUEEN_PSQT_MIDDLE_GAME: [i32; 64] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 20, 20, 13,
    0, 0, 0, 0, 20, 25, 25, 20, 0, 0, 0, 0, 13, 20, 20, 13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
];

const KING_PSQT_MIDDLE_GAME: [i32; 64] = [
    100, 120, 80, 60, 60, 80, 120, 100, 90, 100, 70, 50, 50, 70, 100, 90, 30, 10, 0, -20, -20, 0,
    10, 30, -30, -40, -60, -80, -80, -60, -40, -30, -80, -90, -100, -120, -120, -100, -90, -80,
    -120, -130, -140, -150, -150, -140, -130, -120, -130, -140, -150, -170, -170, -150, -140, -130,
    -170, -180, -190, -200, -200, -190, -180, -170,
];

const KNIGHT_MOBILITY: [i32; 64] = [
    10, 11, 12, 13, 13, 11, 11, 11, 12, 13, 14, 15, 15, 14, 13, 12, 14, 20, 22, 24, 24, 22, 20, 14,
    16, 24, 26, 28, 28, 26, 24, 16, 18, 26, 28, 30, 30, 28, 26, 18, 20, 28, 30, 32, 32, 30, 28, 20,
    16, 17, 18, 19, 19, 18, 17, 16, 12, 13, 14, 15, 15, 14, 13, 12,
];

const BISHOP_MOBILITY: [i32; 64] = [
    8, 9, 10, 11, 11, 10, 2, 8, 10, 11, 12, 13, 13, 12, 11, 10, 12, 17, 18, 21, 21, 18, 17, 12, 14,
    21, 23, 25, 25, 23, 21, 14, 16, 23, 25, 26, 26, 25, 23, 16, 17, 25, 27, 28, 28, 27, 25, 17, 14,
    15, 16, 17, 17, 16, 15, 14, 10, 11, 12, 13, 13, 12, 11, 10,
];

const ROOK_MOBILITY: [i32; 64] = [
    4, 5, 6, 7, 7, 6, 5, 4, 5, 6, 7, 8, 8, 7, 6, 5, 7, 8, 9, 10, 10, 9, 8, 7, 9, 10, 11, 12, 12,
    11, 10, 9, 11, 12, 13, 14, 14, 13, 12, 11, 13, 14, 15, 16, 16, 15, 14, 13, 15, 16, 17, 18, 18,
    17, 16, 15, 11, 12, 13, 14, 14, 13, 12, 11,
];

const QUEEN_MOBILITY: [i32; 64] = [
    2, 3, 3, 4, 4, 3, 3, 2, 2, 3, 3, 4, 4, 3, 3, 2, 3, 4, 5, 5, 5, 5, 4, 3, 4, 5, 5, 6, 6, 5, 5, 4,
    5, 6, 6, 7, 7, 6, 6, 5, 6, 7, 7, 8, 8, 7, 7, 6, 5, 5, 5, 6, 6, 5, 5, 5, 3, 3, 3, 4, 4, 3, 3, 3,
];

fn get_from_psqt(psqt: &[i32; 64], square: board::Square, owner: board::Player) -> i32 {
    let position = match owner {
        board::Player::Black => 8 * (7 - square.rank) + square.file,
        board::Player::White => 8 * square.rank + square.file,
    };
    psqt[position as usize]
}

fn get_mobility(psqt: &[i32; 64], p: board::Player, moves: Vec<moves::Move>) -> i32 {
    moves.iter().fold(0, |acc, mv| {
        let (file, rank) = match mv {
            moves::Move::Castle(_) => (0, 0),
            moves::Move::EnPassent(_) => (0, 0),
            moves::Move::Move(_, dst, _) => (dst.file, dst.file),
        };
        match p {
            board::Player::Black => acc + psqt[(8 * (7 - rank) + file) as usize],
            board::Player::White => acc + psqt[(8 * rank + file) as usize],
        }
    })
}

fn piece_value(
    board: &board::Board,
    square: board::Square,
    piece: board::Piece,
    stage: GameStage,
) -> i32 {
    use board::PieceKind::*;
    match piece.kind {
        Pawn => match stage {
            GameStage::MiddleGame => {
                PAWN_VALUE_MIDDLE_GAME + get_from_psqt(&PAWN_PSQT_MIDDLE_GAME, square, piece.owner)
            }
            GameStage::EndGame => PAWN_VALUE_END_GAME,
        },
        Knight => match stage {
            GameStage::MiddleGame => {
                KNIGHT_VALUE_MIDDLE_GAME
                    + get_from_psqt(&KNIGHT_PSQT_MIDDLE_GAME, square, piece.owner)
                    + get_mobility(
                        &KNIGHT_MOBILITY,
                        piece.owner,
                        get_legal_moves(board, square).unwrap(),
                    )
            }
            GameStage::EndGame => KNIGHT_VALUE_END_GAME,
        },
        Bishop => match stage {
            GameStage::MiddleGame => {
                BISHOP_VALUE_MIDDLE_GAME
                    + get_from_psqt(&BISHOP_PSQT_MIDDLE_GAME, square, piece.owner)
                    + get_mobility(
                        &BISHOP_MOBILITY,
                        piece.owner,
                        get_legal_moves(board, square).unwrap(),
                    )
            }
            GameStage::EndGame => BISHOP_VALUE_END_GAME,
        },
        Rook => match stage {
            GameStage::MiddleGame => {
                ROOK_VALUE_MIDDLE_GAME
                    + get_from_psqt(&ROOK_PSQT_MIDDLE_GAME, square, piece.owner)
                    + get_mobility(
                        &ROOK_MOBILITY,
                        piece.owner,
                        get_legal_moves(board, square).unwrap(),
                    )
            }
            GameStage::EndGame => ROOK_VALUE_END_GAME,
        },
        Queen => match stage {
            GameStage::MiddleGame => {
                QUEEN_VALUE_MIDDLE_GAME
                    + get_from_psqt(&QUEEN_PSQT_MIDDLE_GAME, square, piece.owner)
                    + get_mobility(
                        &QUEEN_MOBILITY,
                        piece.owner,
                        get_legal_moves(board, square).unwrap(),
                    )
            }
            GameStage::EndGame => QUEEN_VALUE_END_GAME,
        },
        King => match stage {
            GameStage::MiddleGame => get_from_psqt(&KING_PSQT_MIDDLE_GAME, square, piece.owner),
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
                    piece,
                    GameStage::MiddleGame,
                ),
            ),
            board::Player::White => (
                wc + piece_value(
                    board,
                    board::Square::from_idx_unsafe(idx as u8),
                    piece,
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
        let mut min = i32::MAX;
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
            (x, mini(&board, 2))
        })
        .max_by(|x, y| x.1.cmp(&y.1))
        .map(|x| x.0)
        .copied()
}

pub fn make_move(board: &mut board::Board) {
    board.make_move(search(board).unwrap());
}
