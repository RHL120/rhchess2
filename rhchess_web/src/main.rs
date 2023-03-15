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
    wasm_logger::init(wasm_logger::Config::default());
    yew::Renderer::<App>::new().render();
}
