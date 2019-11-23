//use crate::lexer::Annot;
use crate::parser::{
    Indirect, Instruction, NumberValue, Opcode, Operand, Operands, ParseError, Parser,
};
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
    NoOperandExpected,
    SingleOperandExpected,
    TwoOperandsExpected,
    UnknownRegisterOrFlag,
    IllegalIndirect,
    IllegalIndexedIndirect,
    IllegalOperand,
    BitRange,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum AnalyzedOperands {
    NoOperand,
    SingleOperand(AnalyzedOperand),
    TwoOperands(AnalyzedOperand, AnalyzedOperand),
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
    Immediate(i32),
}

fn parse_number(s: &str) -> Result<i32, AssembleError> {
    let s = s.to_ascii_lowercase();
    let n = s.len();
    if s.ends_with('h') {
        i32::from_str_radix(&s[..(n - 1)], 16).map_err(AssembleError::ParseIntError)
    } else if s.ends_with('b') {
        i32::from_str_radix(&s[..(n - 1)], 2).map_err(AssembleError::ParseIntError)
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
        },
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
        },
        Operand::Indirect(Indirect::IndexedPlus(s, NumberValue::Literal(n))) => {
            match s.value.to_ascii_lowercase().as_str() {
                "ix" => Ok(AnalyzedOperand::IndirectIX(parse_number(&n.value)?)),
                "iy" => Ok(AnalyzedOperand::IndirectIY(parse_number(&n.value)?)),
                _ => Err(AssembleError::IllegalIndexedIndirect),
            }
        }
        Operand::Indirect(Indirect::IndexedMinus(s, NumberValue::Literal(n))) => {
            match s.value.to_ascii_lowercase().as_str() {
                "ix" => Ok(AnalyzedOperand::IndirectIX(-parse_number(&n.value)?)),
                "iy" => Ok(AnalyzedOperand::IndirectIY(-parse_number(&n.value)?)),
                _ => Err(AssembleError::IllegalIndexedIndirect),
            }
        }
        Operand::Indirect(Indirect::Immediate(NumberValue::Literal(n))) => {
            Ok(AnalyzedOperand::Indirect(parse_number(&n.value)?))
        }
    }
}

fn is_reg8(r: &AnalyzedOperand) -> bool {
    match r {
        AnalyzedOperand::A
        | AnalyzedOperand::B
        | AnalyzedOperand::C
        | AnalyzedOperand::D
        | AnalyzedOperand::E
        | AnalyzedOperand::H
        | AnalyzedOperand::L => true,
        _ => false,
    }
}

fn is_reg16(rr: &AnalyzedOperand) -> bool {
    match rr {
        AnalyzedOperand::BC | AnalyzedOperand::DE | AnalyzedOperand::HL | AnalyzedOperand::SP => {
            true
        }
        _ => false,
    }
}

fn is_reg16xy(rr: &AnalyzedOperand) -> bool {
    match rr {
        AnalyzedOperand::BC
        | AnalyzedOperand::DE
        | AnalyzedOperand::HL
        | AnalyzedOperand::SP
        | AnalyzedOperand::IX
        | AnalyzedOperand::IY => true,
        _ => false,
    }
}

fn is_ind_hlxy(r: &AnalyzedOperand) -> bool {
    match r {
        AnalyzedOperand::IndirectHL
        | AnalyzedOperand::IndirectIX(_)
        | AnalyzedOperand::IndirectIY(_)
        | AnalyzedOperand::IndirectIX0
        | AnalyzedOperand::IndirectIY0 => true,
        _ => false,
    }
}

fn is_cond(c: &AnalyzedOperand) -> bool {
    match c {
        AnalyzedOperand::C
        | AnalyzedOperand::NC
        | AnalyzedOperand::Z
        | AnalyzedOperand::NZ
        | AnalyzedOperand::P
        | AnalyzedOperand::M
        | AnalyzedOperand::PE
        | AnalyzedOperand::PO => true,
        _ => false,
    }
}

fn lower_byte(n: u16) -> u8 {
    (n & 0xff) as u8
}

fn upper_byte(n: u16) -> u8 {
    (n >> 8) as u8
}

fn gen1(c: u8) -> CodeChunk {
    CodeChunk::new(vec![c])
}

fn gen2(c1: u8, c2: u8) -> CodeChunk {
    CodeChunk::new(vec![c1, c2])
}

fn gen1_imm16(c: u8, n: u16) -> CodeChunk {
    CodeChunk::new(vec![c, lower_byte(n), upper_byte(n)])
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

fn gen_ind_hlx2(c1: u8, c2: u8, r: AnalyzedOperand) -> CodeChunk {
    CodeChunk::new(match r {
        AnalyzedOperand::IndirectHL => vec![c1, c2],
        AnalyzedOperand::IndirectIX(o) => vec![0xdd, c1, o as u8, c2],
        AnalyzedOperand::IndirectIY(o) => vec![0xfd, c1, o as u8, c2],
        AnalyzedOperand::IndirectIX0 => vec![0xdd, c1, 0x00, c2],
        AnalyzedOperand::IndirectIY0 => vec![0xfd, c1, 0x00, c2],
        _ => panic!("indirect hlx expected"),
    })
}

fn gen_reg16xy(c: u8, rr: AnalyzedOperand) -> CodeChunk {
    CodeChunk::new(match rr {
        AnalyzedOperand::BC => vec![c],
        AnalyzedOperand::DE => vec![c + 0x10],
        AnalyzedOperand::HL => vec![c + 0x20],
        AnalyzedOperand::SP => vec![c + 0x30],
        AnalyzedOperand::IX => vec![0xdd, c + 0x20],
        AnalyzedOperand::IY => vec![0xfd, c + 0x20],
        _ => panic!("reg16xy expected"),
    })
}

fn reg8_shift(c: u8, r: AnalyzedOperand, s: i32) -> u8 {
    match r {
        AnalyzedOperand::A => c + (7 << s),
        AnalyzedOperand::B => c,
        AnalyzedOperand::C => c + (1 << s),
        AnalyzedOperand::D => c + (2 << s),
        AnalyzedOperand::E => c + (3 << s),
        AnalyzedOperand::H => c + (4 << s),
        AnalyzedOperand::L => c + (5 << s),
        _ => panic!("reg8 expected"),
    }
}

fn reg8(c: u8, r: AnalyzedOperand) -> u8 {
    reg8_shift(c, r, 0)
}

fn reg8_3(c: u8, r: AnalyzedOperand) -> u8 {
    reg8_shift(c, r, 3)
}

fn reg16(c: u8, rr: AnalyzedOperand) -> u8 {
    match rr {
        AnalyzedOperand::BC => c,
        AnalyzedOperand::DE => c + 0x10,
        AnalyzedOperand::HL => c + 0x20,
        AnalyzedOperand::SP => c + 0x30,
        _ => panic!("reg16 expected"),
    }
}

fn bit(c: u8, b: i32) -> Result<u8, AssembleError> {
    if b < 0 || 7 < b {
        Err(AssembleError::BitRange)
    } else {
        Ok(c + b as u8 * 8)
    }
}

fn cond(c1: u8, c: AnalyzedOperand) -> u8 {
    c1 + match c {
        AnalyzedOperand::NZ => 0x00,
        AnalyzedOperand::Z => 0x08,
        AnalyzedOperand::NC => 0x10,
        AnalyzedOperand::C => 0x18,
        AnalyzedOperand::PO => 0x20,
        AnalyzedOperand::PE => 0x28,
        AnalyzedOperand::P => 0x30,
        AnalyzedOperand::M => 0x38,
        _ => panic!("cond expected"),
    }
}

type AO = AnalyzedOperand;

fn analyze_operands(operands: Operands) -> Result<AnalyzedOperands, AssembleError> {
    match operands {
        Operands::NoOperand => Ok(AnalyzedOperands::NoOperand),
        Operands::SingleOperand(opr) => Ok(AnalyzedOperands::SingleOperand(analyze_operand(opr)?)),
        Operands::TwoOperands(opr1, opr2) => Ok(AnalyzedOperands::TwoOperands(
            analyze_operand(opr1)?,
            analyze_operand(opr2)?,
        )),
    }
}

fn expect_no_operand(operands: Operands) -> Result<(), AssembleError> {
    match analyze_operands(operands)? {
        AnalyzedOperands::NoOperand => Ok(()),
        _ => Err(AssembleError::NoOperandExpected),
    }
}

fn expect_single_operand(operands: Operands) -> Result<AnalyzedOperand, AssembleError> {
    match analyze_operands(operands)? {
        AnalyzedOperands::SingleOperand(opr) => Ok(opr),
        _ => Err(AssembleError::SingleOperandExpected),
    }
}

fn expect_two_operands(
    operands: Operands,
) -> Result<(AnalyzedOperand, AnalyzedOperand), AssembleError> {
    match analyze_operands(operands)? {
        AnalyzedOperands::TwoOperands(opr1, opr2) => Ok((opr1, opr2)),
        _ => Err(AssembleError::TwoOperandsExpected),
    }
}

fn assemble_acc_operation(operands: Operands, op_base: u8) -> Result<CodeChunk, AssembleError> {
    match expect_single_operand(operands)? {
        ii if is_ind_hlxy(&ii) => Ok(gen_ind_hlx1(op_base + 0x06, ii)),
        AO::Immediate(n) => Ok(gen2(op_base + 0x46, n as u8)),
        r if is_reg8(&r) => Ok(gen1(reg8(op_base, r))),
        _ => Err(AssembleError::IllegalOperand),
    }
}

fn assemble_no_operand1(operands: Operands, c: u8) -> Result<CodeChunk, AssembleError> {
    expect_no_operand(operands)?;
    Ok(gen1(c))
}

fn assemble_no_operand2(operands: Operands, c1: u8, c2: u8) -> Result<CodeChunk, AssembleError> {
    expect_no_operand(operands)?;
    Ok(gen2(c1, c2))
}

fn assemble_adc(operands: Operands) -> Result<CodeChunk, AssembleError> {
    match expect_two_operands(operands)? {
        (AO::A, ii) if is_ind_hlxy(&ii) => Ok(gen_ind_hlx1(0x8e, ii)),
        (AO::A, r) if is_reg8(&r) => Ok(gen1(reg8(88, r))),
        (AO::A, AO::Immediate(n)) => Ok(gen2(0xce, n as u8)),
        (AO::HL, rr) if is_reg16(&rr) => Ok(gen2(0xed, reg16(0x4a, rr))),
        _ => Err(AssembleError::IllegalOperand),
    }
}

fn assemble_add(operands: Operands) -> Result<CodeChunk, AssembleError> {
    match expect_two_operands(operands)? {
        (AO::A, ii) if is_ind_hlxy(&ii) => Ok(gen_ind_hlx1(0x86, ii)),
        (AO::A, r) if is_reg8(&r) => Ok(gen1(reg8(80, r))),
        (AO::A, AO::Immediate(n)) => Ok(gen2(0xc6, n as u8)),
        (AO::HL, rr) if is_reg16(&rr) => Ok(gen1(reg16(0x09, rr))),
        (AO::IX, AO::BC) => Ok(gen2(0xdd, 0x09)),
        (AO::IX, AO::DE) => Ok(gen2(0xdd, 0x19)),
        (AO::IX, AO::IX) => Ok(gen2(0xdd, 0x29)),
        (AO::IX, AO::SP) => Ok(gen2(0xdd, 0x39)),
        (AO::IY, AO::BC) => Ok(gen2(0xfd, 0x09)),
        (AO::IY, AO::DE) => Ok(gen2(0xfd, 0x19)),
        (AO::IY, AO::IY) => Ok(gen2(0xfd, 0x29)),
        (AO::IY, AO::SP) => Ok(gen2(0xfd, 0x39)),
        _ => Err(AssembleError::IllegalOperand),
    }
}

fn assemble_bit(operands: Operands) -> Result<CodeChunk, AssembleError> {
    match expect_two_operands(operands)? {
        (AO::Immediate(b), ii) if is_ind_hlxy(&ii) => Ok(gen_ind_hlx2(0xcb, bit(0x46, b)?, ii)),
        (AO::Immediate(b), r) if is_reg8(&r) => Ok(gen2(0xcb, bit(reg8(0x40, r), b)?)),
        _ => Err(AssembleError::IllegalOperand),
    }
}

fn assemble_call(operands: Operands) -> Result<CodeChunk, AssembleError> {
    match analyze_operands(operands)? {
        AnalyzedOperands::SingleOperand(AO::Immediate(n)) => Ok(gen1_imm16(0xcd, n as u16)),
        AnalyzedOperands::TwoOperands(c, AO::Immediate(n)) if is_cond(&c) => {
            Ok(gen1_imm16(cond(0xc4, c), n as u16))
        }
        _ => Err(AssembleError::IllegalOperand),
    }
}

fn assemble_dec(operands: Operands) -> Result<CodeChunk, AssembleError> {
    match expect_single_operand(operands)? {
        ii if is_ind_hlxy(&ii) => Ok(gen_ind_hlx1(0x35, ii)),
        r if is_reg8(&r) => Ok(gen1(reg8_3(0x05, r))),
        rr if is_reg16xy(&rr) => Ok(gen_reg16xy(0x0b, rr)),
        _ => Err(AssembleError::IllegalOperand),
    }
}

fn assemble_machine_instruction(
    opcode: Opcode,
    operands: Operands,
) -> Result<CodeChunk, AssembleError> {
    match opcode.0 {
        "adc" => assemble_adc(operands),
        "add" => assemble_add(operands),
        "and" => assemble_acc_operation(operands, 0xa0),
        "bit" => assemble_bit(operands),
        "call" => assemble_call(operands),
        "ccf" => assemble_no_operand1(operands, 0xcf),
        "cp" => assemble_acc_operation(operands, 0xb8),
        "cpd" => assemble_no_operand2(operands, 0xed, 0xa9),
        "cpdr" => assemble_no_operand2(operands, 0xed, 0xb9),
        "cpi" => assemble_no_operand2(operands, 0xed, 0xa1),
        "cpir" => assemble_no_operand2(operands, 0xed, 0xb1),
        "cpl" => assemble_no_operand1(operands, 0x2f),
        "daa" => assemble_no_operand1(operands, 0x27),
        "dec" => assemble_dec(operands),
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
