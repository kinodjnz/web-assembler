use crate::assembler::parse_and_assemble;
use crate::parser::Parser;
use crate::emulator::Emulator;
use yew::{html, Component, ComponentLink, Html, ShouldRender};

pub struct Model {
    value: String,
    code: String,
    step: String,
    reg: String,
    emulator: Emulator,
}

pub enum Msg {
    Assemble,
    TextChanged(String),
}

impl Model {
    fn assemble_and_step(&mut self) -> Result<(), ()> {
        let mut parser = Parser::new(self.value.chars());
        let code = parse_and_assemble(&mut parser)
            .map_err(|other| {
                self.code = format!("{:?}", other);
            })?;
        self.emulator.load(&code, 0x0100);
        self.code = code.to_hex();
        let step = self.emulator.step();
        self.step = format!("{:?}", step);
        self.reg = self.emulator.show_reg();
        Ok(())
    }
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        Model {
            value: String::new(),
            code: String::new(),
            step: String::new(),
            reg: String::new(),
            emulator: Emulator::new(),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::Assemble => {
                self.assemble_and_step();
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
                <div><code>{self.step.clone()}</code></div>
                <div><code>{self.reg.clone()}</code></div>
            </div>
        }
    }
}
