use crate::board;

#[derive(Debug, PartialEq, Eq)]
enum Score {
    Normal(i32),
    Checkmate,
    StaleMate,
}

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
            GameStage::MiddleGame => todo!(),
            GameStage::EndGame => todo!(),
        },
    }
}

fn evaluate(board: &board::Board, p: board::Player) -> Score {
    use Score::*;
    let king_pos = match p {
        board::Player::White => board.white_pos,
        board::Player::Black => board.black_pos,
    };
    if board.attacks.does_attack(p.opposite(), king_pos) {
        return Checkmate;
    }
    StaleMate
}
