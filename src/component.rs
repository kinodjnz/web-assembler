use crate::assembler::parse_and_assemble;
use crate::emulator::Emulator;
use crate::parser::Parser;
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
    Step,
}

impl Model {
    fn assemble(&mut self) -> Result<(), ()> {
        let mut parser = Parser::new(self.value.chars());
        let code = parse_and_assemble(&mut parser).map_err(|other| {
            self.code = format!("{:?}", other);
        })?;
        self.emulator.reset();
        self.emulator.load(&code, 0x0100);
        self.code = code.to_hex();
        Ok(())
    }

    fn step(&mut self) -> Result<(), ()> {
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
                self.assemble().unwrap_or(());
                true
            }
            Msg::TextChanged(value) => {
                self.value = value;
                true
            }
            Msg::Step => {
                self.step().unwrap_or(());
                true
            }
        }
    }

    fn view(&self) -> Html<Self> {
        html! {
            <div>
                <div>
                    <textarea
                      rows="10"
                      cols="40"
                      value=&self.value
                      oninput=|e| Msg::TextChanged(e.value)>
                    </textarea>
                </div>
                <button onclick=|_| Msg::Assemble>{ "assemble" }</button>
                <div><code>{self.code.clone()}</code></div>
                <button onclick=|_| Msg::Step>{ "step" }</button>
                <div><code>{self.step.clone()}</code></div>
                <div><code>{self.reg.clone()}</code></div>
            </div>
        }
    }
}
