use yew::{html, Component, ComponentLink, Html, ShouldRender};
use crate::assembler::Assembler;

pub struct Model {
    value: String,
 }

pub enum Msg {
    Assemble,
    TextChanged(String),
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        Model { value: String::new() }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::Assemble => {
                // Update your model on events
                //self.value = "Hello, world".into();
                let mut assembler = Assembler::new(self.value.chars());
                self.value = format!("{:?}", assembler.parse_next());
                true
            }
            Msg::TextChanged(value) => {
                self.value = value;
                true
            }
        }
    }

    fn view(&self) -> Html<Self> {
        html! {
            // Render your model here
            <div>
                <button onclick=|_| Msg::Assemble>{ "assemble" }</button>
                <div>
                    <textarea
                      rows="10"
                      cols="40"
                      value=&self.value
                      oninput=|e| Msg::TextChanged(e.value)>
                    </textarea>
                    //<input value=self.value oninput=|e| Msg::TextChanged(e.value)></input>
                </div>
            </div>
        }
    }
}
