use crate::lexer::{Annot, LexError, Lexer, Token, TokenKind};
use crate::loc::Loc;

#[derive(Debug)]
pub struct Parser<I: Iterator<Item = char>> {
    lexer: Lexer<I>,
    token: Option<Token>,
    loc: Loc,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum ParseError {
    LexError(LexError),
    UnknownInstruction(Token),
    UnexpectedToken(Token),
    OperandExpected(Token),
    NumberExpected(Token),
    TokenExpected(Annot<String>),
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum NumberValue {
    Literal(Annot<String>),
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum Indirect {
    Register(Annot<String>),
    IndexedPlus(Annot<String>, NumberValue),
    IndexedMinus(Annot<String>, NumberValue),
    Immediate(NumberValue),
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum Operand {
    Register(Annot<String>),
    Literal(Annot<String>),
    Indirect(Indirect),
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum Operands {
    NoOperand,
    SingleOperand(Operand),
    TwoOperands(Operand, Operand),
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Opcode(pub &'static str);

const OPCODES: [&str; 67] = [
    "adc", "add", "and", "bit", "call", "ccf", "cp", "cpd", "cpdr", "cpi", "cpir", "cpl", "daa",
    "dec", "di", "djnz", "ei", "ex", "exx", "halt", "im", "in", "inc", "ind", "indr", "ini",
    "inir", "jp", "jr", "ld", "ldd", "lddr", "ldi", "ldir", "neg", "nop", "or", "otdr", "otir",
    "out", "outd", "outi", "pop", "push", "res", "ret", "reti", "retn", "rl", "rla", "rlc", "rlca",
    "rld", "rr", "rra", "rrc", "rrca", "rrd", "rst", "sbc", "scf", "set", "sla", "sra", "srl",
    "sub", "xor",
];

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum Instruction {
    Machine(Opcode, Operands),
}

impl<I: Iterator<Item = char>> Parser<I> {
    pub fn new(iter: I) -> Self {
        Parser {
            lexer: Lexer::new(iter),
            token: None,
            loc: Loc::default(),
        }
    }

    fn fill_nonempty_token(&mut self) -> Result<(), ParseError> {
        while let Some(token) = self.next_token()? {
            if token.value == TokenKind::EndOfLine {
                continue;
            }
            self.loc = token.loc;
            self.token = Some(token);
            return Ok(());
        }
        self.token = None;
        Ok(())
    }

    fn next_token(&mut self) -> Result<Option<Token>, ParseError> {
        self.lexer.next_token().map_err(ParseError::LexError)
    }

    fn fill_token(&mut self) -> Result<(), ParseError> {
        if let Some(token) = self.next_token()? {
            if token.value == TokenKind::EndOfLine {
                self.loc = token.loc;
                self.token = None;
            } else {
                self.loc = token.loc;
                self.token = Some(token);
            }
        } else {
            self.token = None;
        };
        Ok(())
    }

    fn expect_empty(&mut self) -> Result<(), ParseError> {
        if let Some(token) = self.token.take() {
            Err(ParseError::UnexpectedToken(token))
        } else {
            Ok(())
        }
    }

    fn expect_symbol(&mut self, s: &str) -> Result<(), ParseError> {
        if let Some(token) = self.token.take() {
            self.fill_token()?;
            match &token.value {
                TokenKind::Symbol(symbol) if symbol == s => Ok(()),
                _ => Err(ParseError::UnexpectedToken(token)),
            }
        } else {
            Err(ParseError::TokenExpected(Annot::new(s.into(), self.loc)))
        }
    }

    fn is_symbol_of(&self, token: &Token, s: &str) -> bool {
        if let TokenKind::Symbol(symbol) = &token.value {
            symbol == s
        } else {
            false
        }
    }

    fn is_next_symbol(&mut self, s: &str) -> Result<bool, ParseError> {
        if let Some(token) = &self.token {
            if self.is_symbol_of(&token, s) {
                self.fill_token()?;
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn parse_number(&mut self) -> Result<Annot<String>, ParseError> {
        if let Some(token) = self.token.take() {
            self.fill_token()?;
            match &token.value {
                TokenKind::Literal(value) => Ok(Annot::new(value.into(), token.loc)),
                _ => Err(ParseError::NumberExpected(token)),
            }
        } else {
            Err(ParseError::TokenExpected(Annot::new(
                "number".into(),
                self.loc,
            )))
        }
    }

    fn parse_register_indirect(&mut self, keyword: Annot<String>) -> Result<Indirect, ParseError> {
        if self.is_next_symbol("+")? {
            let number = self.parse_number()?;
            Ok(Indirect::IndexedPlus(keyword, NumberValue::Literal(number)))
        } else if self.is_next_symbol("-")? {
            let number = self.parse_number()?;
            Ok(Indirect::IndexedMinus(
                keyword,
                NumberValue::Literal(number),
            ))
        } else {
            Ok(Indirect::Register(keyword))
        }
    }

    fn parse_indirect(&mut self) -> Result<Indirect, ParseError> {
        let token = self.token.take();
        self.fill_token()?;
        match token {
            Some(token) => match &token.value {
                TokenKind::Keyword(s) => {
                    self.parse_register_indirect(Annot::new(s.into(), token.loc))
                }
                TokenKind::Literal(s) => Ok(Indirect::Immediate(NumberValue::Literal(Annot::new(
                    s.into(),
                    token.loc,
                )))),
                _ => Err(ParseError::UnexpectedToken(token.clone())),
            },
            _ => Err(ParseError::TokenExpected(Annot::new(
                "register".into(),
                self.loc,
            ))),
        }
    }

    fn parse_operand(&mut self, token: Token) -> Result<Operand, ParseError> {
        self.fill_token()?;
        match &token.value {
            TokenKind::Keyword(s) => Ok(Operand::Register(Annot::new(s.into(), token.loc))),
            TokenKind::Literal(s) => Ok(Operand::Literal(Annot::new(s.into(), token.loc))),
            TokenKind::Symbol(s) if s == "(" => {
                let indirect = self.parse_indirect()?;
                self.expect_symbol(")")?;
                Ok(Operand::Indirect(indirect))
            }
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    fn parse_operands(&mut self) -> Result<Operands, ParseError> {
        match self.token.take() {
            Some(token) => {
                let op = self.parse_operand(token)?;
                match self.token.take() {
                    Some(token) if self.is_symbol_of(&token, ",") => {
                        self.fill_token()?;
                        if let Some(token) = self.token.take() {
                            let op2 = self.parse_operand(token)?;
                            self.expect_empty()?;
                            Ok(Operands::TwoOperands(op, op2))
                        } else {
                            Err(ParseError::OperandExpected(token))
                        }
                    }
                    Some(token) => Err(ParseError::UnexpectedToken(token)),
                    _ => Ok(Operands::SingleOperand(op)),
                }
            }
            _ => Ok(Operands::NoOperand),
        }
    }

    fn parse_opcode(&mut self) -> Result<Option<Opcode>, ParseError> {
        if let Some(token) = &self.token {
            if let TokenKind::Keyword(s) = &token.value {
                if let Ok(i) = OPCODES.binary_search(&s.as_str()) {
                    self.fill_token()?;
                    return Ok(Some(Opcode(OPCODES[i])));
                }
            }
        }
        Ok(None)
    }

    pub fn parse_instruction(&mut self) -> Result<Option<Instruction>, ParseError> {
        self.fill_nonempty_token()?;
        if let Some(opcode) = self.parse_opcode()? {
            let operands = self.parse_operands()?;
            Ok(Some(Instruction::Machine(opcode, operands)))
        } else if let Some(token) = self.token.take() {
            Err(ParseError::UnknownInstruction(token))
        } else {
            Ok(None)
        }
    }
}
