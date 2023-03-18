use rhchess_engine::board;
use yew::prelude::*;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub on_choice: Callback<board::PieceKind>,
    pub player: board::Player,
}

#[function_component]
pub fn PromotionMenu(props: &Props) -> Html {
    use board::PieceKind::*;
    let player = match props.player {
        board::Player::Black => 'b',
        board::Player::White => 'w',
    };
    html! {
        <div id="promotion_menu">
        {
            [('B', Bishop), ('N', Knight), ('Q', Queen), ('R', Rook)]
                .iter()
                .map(|&(name, kind)| {
                    let on_choice = props.on_choice.clone();
                    let onclick = move |_: MouseEvent| on_choice.emit(kind);
                    let img = format!("piece_set/{}{}.svg", player, name);
                    html! {
                        <img src={img} {onclick}/>
                    }
                }).collect::<Html>()
        }
        </div>
    }
}
