use crate::parser::Parser;
use yew::{html, Component, ComponentLink, Html, ShouldRender};

pub struct Model {
    value: String,
    code: String,
}

pub enum Msg {
    Assemble,
    TextChanged(String),
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        Model {
            value: String::new(),
            code: String::new(),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::Assemble => {
                let mut parser = Parser::new(self.value.chars());
                self.code = format!("{:?}", parser.parse_instruction());
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
                <div><code>{self.code.clone()}</code></div>
            </div>
        }
    }
}
