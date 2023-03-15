use super::square;
use super::square::Square;
use rhchess_engine::board;
use rhchess_engine::moves;
use std::sync::Arc;
use std::sync::Mutex;
use yew::prelude::*;
#[function_component]
pub fn Board() -> Html {
    let selected = use_state::<Option<board::Square>, _>(|| None);
    let targets = use_state::<Vec<moves::Move>, _>(|| Vec::new());
    let ref_board = use_state(|| Arc::new(Mutex::new(board::Board::default())));
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
                            _ => false
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
                            match targets.iter().position(target_check) {
                                Some(m) => Callback::from(move |_| {
                                    targets.set(Vec::new());
                                    let mut b = board.lock().unwrap();
                                    b.make_move(targets[m]);
                                    b.switch_player();
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
        </div>
    };
    ret
}
