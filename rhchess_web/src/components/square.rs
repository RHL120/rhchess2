use rhchess_engine::board;
use yew::prelude::*;
#[derive(Clone, Copy, PartialEq)]
pub enum Color {
    Light,
    Dark,
}

#[derive(PartialEq)]
pub enum Status {
    Targeted,
    Selected,
    Unselected,
}

#[derive(Properties, PartialEq)]
pub struct Props {
    pub color: Color,
    pub piece: Option<board::Piece>,
    pub status: Status,
    pub on_click: Callback<MouseEvent>,
}

#[function_component]
pub fn Square(props: &Props) -> Html {
    let mut color = match props.color {
        Color::Light => (210, 180, 145),
        Color::Dark => (157, 111, 75),
    };
    match props.status {
        Status::Targeted => color.0 += 30,
        Status::Selected => color.1 += 30,
        _ => (),
    };
    html! {
        <div class="square" style={format!("background-color: rgb({}, {}, {})", color.0, color.1, color.2)} onclick={props.on_click.clone()}>
            {
                if let Some(piece) = props.piece {
                    let color = match piece.owner {
                        board::Player::White => 'w',
                        board::Player::Black => 'b',
                    };
                    let piece = match piece.kind {
                        board::PieceKind::Pawn => 'P',
                        board::PieceKind::Rook => 'R',
                        board::PieceKind::Knight => 'N',
                        board::PieceKind::Bishop => 'B',
                        board::PieceKind::Queen => 'Q',
                        board::PieceKind::King => 'K',
                    };
                    html! {
                        <img class="square--image" src={format!("piece_set/{}{}.svg", color, piece)}/>
                    }
                }  else {
                    html!{}
                }
            }
        </div>
    }
}
