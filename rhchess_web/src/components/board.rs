use super::promotion::PromotionMenu;
use super::square;
use super::square::Square;
use rhchess_engine::board;
use rhchess_engine::moves;
use std::sync::Arc;
use std::sync::Mutex;
use yew::prelude::*;
#[function_component]
pub fn Board() -> Html {
    //TODO: The code found below is known to cause headaches, chess AIDS, and lung cancer
    //for those who look at it. I should probably replace it with something
    //inspired by elm.
    let selected = use_state::<Option<board::Square>, _>(|| None);
    let targets = use_state::<Vec<moves::Move>, _>(Vec::new);
    let ref_board = use_state(|| Arc::new(Mutex::new(board::Board::default())));
    let promotion = use_state::<Option<Callback<board::PieceKind>>, _>(|| None);
    let ret = html! {
        <div id="board">
            {
                (0..64).map(|idx| {
                    let board = ref_board.clone();
                    let square = board::Square::new(idx % 8, 7 - (idx / 8)).unwrap();
                    let piece = board.lock().unwrap().get_piece(square);
                    let color = if  (idx % 8 + idx / 8) % 2 == 0 {
                        square::Color::Light
                    } else {
                        square::Color::Dark
                    };
                    let target_check = |&i: &moves::Move| {
                        match i {
                            moves::Move::Move(_, dst, _) => square == dst,
                            moves::Move::EnPassent(_) => {
                                let board = board.lock().unwrap();
                                let (_, direction) = board.turn.pawn_info();
                                board.en_passant.unwrap().translate(0, direction).unwrap() == square
                            },
                            moves::Move::Castle(king_side) => {
                                let rank = match board.lock().unwrap().turn {
                                    board::Player::White => 0,
                                    board::Player::Black => 7,
                                };
                                (king_side && square == board::Square::new(6, rank).unwrap())
                                    || (!king_side && square == board::Square::new(2, rank).unwrap())
                            }
                        }
                    };
                    let status = if Some(square) == *selected {
                        square::Status::Selected
                    } else if targets.iter().any(target_check) {
                        square::Status::Targeted
                    } else {
                        square::Status::Unselected
                    };
                    let selected = selected.clone();
                    let targets = targets.clone();
                    let b2 = ref_board.clone();
                    let board = ref_board.clone();
                    let on_click = match piece.map(|x| x.owner == board.lock().unwrap().turn) {
                        Some(true) => Callback::from(move |_| {
                            targets.set(moves::get_move(&b2.lock().unwrap(), square).unwrap());
                            selected.set(Some(square));
                        }),
                        _ => {
                            let promotion = promotion.clone();
                            match targets.iter().position(target_check) {
                                Some(m) => Callback::from(move |_| {
                                    let mut b = board.lock().unwrap();
                                    let promoting = b.is_promotion(targets[m]);
                                    let b2 = board.clone();
                                    b.make_move(targets[m]);
                                    if promoting {
                                        let promotion2 = promotion.clone();
                                        let targets = targets.clone();
                                        promotion.set(Some(Callback::from(move |k| {
                                            targets.set(Vec::new());
                                            let mut board = b2.lock().unwrap();
                                            board.make_promotion(targets[m], k);
                                            board.switch_player();
                                            promotion2.set(None);
                                        })))
                                    } else {
                                        targets.set(Vec::new());
                                        b.switch_player();
                                    }
                                }),
                                None => Callback::from(|_| {})
                            }
                        }
                    };

                    html! {
                        <Square
                            {color}
                            piece={piece}
                            {status}
                            {on_click}
                        />
                    }
                })
                .collect::<Html>()
            }
            {
                if let Some(on_choice) = promotion.as_ref() {
                    html! {
                        <PromotionMenu
                        {on_choice}
                        player={ref_board.lock().unwrap().turn}
                        />
                    }
                } else {
                    html!{}
                }
            }
        </div>
    };
    ret
}
