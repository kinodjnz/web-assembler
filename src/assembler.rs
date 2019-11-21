//use crate::lexer::Annot;
use crate::parser::{Indirect, Instruction, Opcode, Operand, Operands, ParseError, Parser, NumberValue};
use std::num::ParseIntError;

#[derive(Debug)]
pub struct CodeChunk {
    code: Vec<u8>,
}

impl CodeChunk {
    pub fn new(code: Vec<u8>) -> Self {
        CodeChunk { code }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum AssembleError {
    ParseError(ParseError),
    ParseIntError(ParseIntError),
    TwoOperandsExpected,
    UnknownRegisterOrFlag,
    IllegalIndirect,
    IllegalIndexedIndirect,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum AnalyzedOperand {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
    I,
    R,
    AF,
    BC,
    DE,
    HL,
    SP,
    IX,
    IY,
    //AFPrime,
    IndirectHL,
    IndirectBC,
    IndirectDE,
    IndirectSP,
    IndirectC,
    IndirectIX(i32),
    IndirectIY(i32),
    Indirect(i32),
    IndirectIX0,
    IndirectIY0,
    NC,
    Z,
    NZ,
    P,
    M,
    PE,
    PO,
    Immediate(i32)
}

fn parse_number(s: &str) -> Result<i32, AssembleError> {
    let s = s.to_ascii_lowercase();
    let n = s.len();
    if s.ends_with("h") {
        i32::from_str_radix(&s[..(n-1)], 16).map_err(AssembleError::ParseIntError)
    } else if s.ends_with("b") {
        i32::from_str_radix(&s[..(n-1)], 2).map_err(AssembleError::ParseIntError)
    } else {
        i32::from_str_radix(&s, 10).map_err(AssembleError::ParseIntError)
    }
}

fn analyze_operand(operand: Operand) -> Result<AnalyzedOperand, AssembleError> {
    match operand {
        Operand::Register(s) => match s.value.to_ascii_lowercase().as_str() {
            "a" => Ok(AnalyzedOperand::A),
            "b" => Ok(AnalyzedOperand::B),
            "c" => Ok(AnalyzedOperand::C),
            "d" => Ok(AnalyzedOperand::D),
            "e" => Ok(AnalyzedOperand::E),
            "f" => Ok(AnalyzedOperand::F),
            "h" => Ok(AnalyzedOperand::H),
            "l" => Ok(AnalyzedOperand::L),
            "i" => Ok(AnalyzedOperand::I),
            "r" => Ok(AnalyzedOperand::R),
            "af" => Ok(AnalyzedOperand::AF),
            "bc" => Ok(AnalyzedOperand::BC),
            "de" => Ok(AnalyzedOperand::DE),
            "hl" => Ok(AnalyzedOperand::HL),
            "sp" => Ok(AnalyzedOperand::SP),
            "ix" => Ok(AnalyzedOperand::IX),
            "iy" => Ok(AnalyzedOperand::IY),
            // AF'
            "nc" => Ok(AnalyzedOperand::NC),
            "z" => Ok(AnalyzedOperand::Z),
            "nz" => Ok(AnalyzedOperand::NZ),
            "p" => Ok(AnalyzedOperand::P),
            "m" => Ok(AnalyzedOperand::M),
            "pe" => Ok(AnalyzedOperand::PE),
            "po" => Ok(AnalyzedOperand::PO),
            _ => Err(AssembleError::UnknownRegisterOrFlag),
        }
        Operand::Literal(s) => Ok(AnalyzedOperand::Immediate(parse_number(&s.value)?)),
        Operand::Indirect(Indirect::Register(s)) => match s.value.to_ascii_lowercase().as_str() {
            "hl" => Ok(AnalyzedOperand::IndirectHL),
            "bc" => Ok(AnalyzedOperand::IndirectBC),
            "de" => Ok(AnalyzedOperand::IndirectDE),
            "sp" => Ok(AnalyzedOperand::IndirectSP),
            "c" => Ok(AnalyzedOperand::IndirectC),
            "ix" => Ok(AnalyzedOperand::IndirectIX0),
            "iy" => Ok(AnalyzedOperand::IndirectIY0),
            _ => Err(AssembleError::IllegalIndirect),
        }
        Operand::Indirect(Indirect::IndexedPlus(s, NumberValue::Literal(n))) => match s.value.to_ascii_lowercase().as_str() {
            "ix" => Ok(AnalyzedOperand::IndirectIX(parse_number(&n.value)?)),
            "iy" => Ok(AnalyzedOperand::IndirectIY(parse_number(&n.value)?)),
            _ => Err(AssembleError::IllegalIndexedIndirect),
        }
        Operand::Indirect(Indirect::IndexedMinus(s, NumberValue::Literal(n))) => match s.value.to_ascii_lowercase().as_str() {
            "ix" => Ok(AnalyzedOperand::IndirectIX(-parse_number(&n.value)?)),
            "iy" => Ok(AnalyzedOperand::IndirectIY(-parse_number(&n.value)?)),
            _ => Err(AssembleError::IllegalIndexedIndirect),
        }
        Operand::Indirect(Indirect::Immediate(NumberValue::Literal(n))) => Ok(AnalyzedOperand::Indirect(parse_number(&n.value)?)),
    }
}

fn is_reg8(r: &AnalyzedOperand) -> bool {
    match r {
        AnalyzedOperand::A => true,
        AnalyzedOperand::B => true,
        AnalyzedOperand::C => true,
        AnalyzedOperand::D => true,
        AnalyzedOperand::E => true,
        AnalyzedOperand::H => true,
        AnalyzedOperand::L => true,
        _ => false,
    }
}

fn is_reg16(rr: &AnalyzedOperand) -> bool {
    match rr {
        AnalyzedOperand::BC => true,
        AnalyzedOperand::DE => true,
        AnalyzedOperand::HL => true,
        AnalyzedOperand::SP => true,
        _ => false,
    }
}

fn is_ind_hlx(r: &AnalyzedOperand) -> bool {
    match r {
        AnalyzedOperand::IndirectHL => true,
        AnalyzedOperand::IndirectIX(_) => true,
        AnalyzedOperand::IndirectIY(_) => true,
        AnalyzedOperand::IndirectIX0 => true,
        AnalyzedOperand::IndirectIY0 => true,
        _ => false,
    }
}

fn gen1(c: u8) -> CodeChunk {
    CodeChunk::new(vec![c])
}

fn gen2(c1: u8, c2: u8) -> CodeChunk {
    CodeChunk::new(vec![c1, c2])
}

fn gen_ind_hlx1(c: u8, r: AnalyzedOperand) -> CodeChunk {
    CodeChunk::new(match r {
        AnalyzedOperand::IndirectHL => vec![c],
        AnalyzedOperand::IndirectIX(o) => vec![0xdd, c, o as u8],
        AnalyzedOperand::IndirectIY(o) => vec![0xfd, c, o as u8],
        AnalyzedOperand::IndirectIX0 => vec![0xdd, c, 0x00],
        AnalyzedOperand::IndirectIY0 => vec![0xfd, c, 0x00],
        _ => panic!("indirect hlx expected"),
    })
}

fn reg8(c: u8, r: AnalyzedOperand) -> u8 {
    match r {
        AnalyzedOperand::A => c+7,
        AnalyzedOperand::B => c+0,
        AnalyzedOperand::C => c+1,
        AnalyzedOperand::D => c+2,
        AnalyzedOperand::E => c+3,
        AnalyzedOperand::H => c+4,
        AnalyzedOperand::L => c+5,
        _ => panic!("reg8 expected"),
    }
}

fn reg16(c: u8, rr: AnalyzedOperand) -> u8 {
    match rr {
        AnalyzedOperand::BC => c + 0x00,
        AnalyzedOperand::DE => c + 0x10,
        AnalyzedOperand::HL => c + 0x20,
        AnalyzedOperand::SP => c + 0x30,
        _ => panic!("reg16 expected"),
    }
}

type AO = AnalyzedOperand;

fn two_operands(operands: Operands) -> Result<Option<(AnalyzedOperand, AnalyzedOperand)>, AssembleError> {
    if let Operands::TwoOperands(opr1, opr2) = operands {
        let opr1 = analyze_operand(opr1)?;
        let opr2 = analyze_operand(opr2)?;
        Ok(Some((opr1, opr2)))
    } else {
        Ok(None)
    }
}

fn expect_two_operands(operands: Operands) -> Result<(AnalyzedOperand, AnalyzedOperand), AssembleError> {
    two_operands(operands)?.map_or_else(|| Err(AssembleError::TwoOperandsExpected), Result::Ok)
}

fn assemble_adc(operands: Operands) -> Result<CodeChunk, AssembleError> {
    match expect_two_operands(operands)? {
        (AO::A, ii) if is_ind_hlx(&ii) => Ok(gen_ind_hlx1(0x8e, ii)),
        (AO::A, r) if is_reg8(&r) => Ok(gen1(reg8(88, r))),
        (AO::A, AO::Immediate(n)) => Ok(gen2(0xce, n as u8)),
        (AO::HL, rr) if is_reg16(&rr) => Ok(gen2(0xed, reg16(0x4a, rr))),
        _ => Ok(CodeChunk::new(vec![1])),
    }
}

fn assemble_machine_instruction(
    opcode: Opcode,
    operands: Operands,
) -> Result<CodeChunk, AssembleError> {
    match opcode.0 {
        "adc" => assemble_adc(operands),
        _ => Ok(CodeChunk { code: vec![2] }),
    }
}

fn assemble(instruction: Instruction) -> Result<CodeChunk, AssembleError> {
    match instruction {
        Instruction::Machine(opcode, operands) => assemble_machine_instruction(opcode, operands),
    }
}

pub fn parse_and_assemble<I: Iterator<Item = char>>(
    parser: &mut Parser<I>,
) -> Result<CodeChunk, AssembleError> {
    let mut output = Vec::new();
    while let Some(instruction) = parser
        .parse_instruction()
        .map_err(AssembleError::ParseError)?
    {
        let code_chunk = assemble(instruction)?;
        output.extend(code_chunk.code.iter());
    }
    Ok(CodeChunk { code: output })
}
