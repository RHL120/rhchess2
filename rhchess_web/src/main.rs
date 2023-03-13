pub mod components;
use components::board::Board;
use yew::prelude::*;

#[function_component]
fn App() -> Html {
    html! {
        <main>
            <Board />
        </main>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}
