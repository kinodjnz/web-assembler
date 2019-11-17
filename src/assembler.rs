use crate::lexer::{LexError, Lexer, Token, TokenKind};
use crate::loc::Loc;

#[derive(Debug)]
pub struct Assembler<I: Iterator<Item = char>> {
    lexer: Lexer<I>,
    token: Option<Token>,
}

#[derive(Debug)]
pub struct CodeChunk {
    codes: Vec<u8>,
    debug: String,
}

impl CodeChunk {
    fn new(codes: Vec<u8>) -> Self {
        CodeChunk {
            codes,
            debug: "".into(),
        }
    }
    fn new_debug(debug: String) -> Self {
        CodeChunk {
            codes: Vec::new(),
            debug,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParseError {
    LexError(LexError),
    //UnknownInstruction(Token),
    UnexpectedToken(Token),
    OperandExpected(Token),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Operand {
    Register(String),
    Literal(String),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Operands {
    NoOperand,
    SingleOperand(Operand),
    TwoOperands(Operand, Operand),
}

impl<I: Iterator<Item = char>> Assembler<I> {
    pub fn new(iter: I) -> Self {
        Assembler {
            lexer: Lexer::new(iter),
            token: None,
        }
    }

    fn fill_nonempty_token(&mut self) -> Result<(), ParseError> {
        while let Some(token) = self.next_token()? {
            if token.value == TokenKind::EndOfLine {
                continue;
            }
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
        self.token = if let Some(token) = self.next_token()? {
            if token.value == TokenKind::EndOfLine {
                None
            } else {
                Some(token)
            }
        } else {
            None
        };
        Ok(())
    }

    fn parse_operand(&mut self, token: Token) -> Result<Operand, ParseError> {
        self.fill_token()?;
        match &token.value {
            TokenKind::Keyword(s) => Ok(Operand::Register(s.clone())),
            TokenKind::Literal(s) => Ok(Operand::Literal(s.clone())),
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    fn expect_empty(&mut self) -> Result<(), ParseError> {
        if let Some(token) = self.token.take() {
            Err(ParseError::UnexpectedToken(token))
        } else {
            Ok(())
        }
    }

    fn is_symbol_of(&mut self, token: &Token, s: &str) -> bool {
        if let TokenKind::Symbol(symbol) = &token.value {
            symbol == s
        } else {
            false
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

    fn parse_instruction(
        &mut self,
        inst: &str,
        _loc: Loc,
    ) -> Result<Option<CodeChunk>, ParseError> {
        match &inst.to_ascii_lowercase()[..] {
            "ld" => {
                self.fill_token()?;
                let operands = self.parse_operands();
                Ok(Some(CodeChunk::new_debug(format!("{:?}", operands))))
            }
            _ => Ok(Some(CodeChunk::new(vec![0]))),
        }
    }

    pub fn parse_next(&mut self) -> Result<Option<CodeChunk>, ParseError> {
        self.fill_nonempty_token()?;
        if let Some(token) = self.token.take() {
            match token.value {
                TokenKind::Keyword(k) => self.parse_instruction(&k, token.loc),
                _ => Err(ParseError::UnexpectedToken(token.clone())),
            }
        } else {
            Ok(None)
        }
    }
}
