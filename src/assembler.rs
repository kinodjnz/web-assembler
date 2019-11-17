use crate::parser::{Instruction, ParseError, Parser};

#[derive(Debug)]
pub struct CodeChunk {
    code: Vec<u8>,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum AssembleError {
    ParseError(ParseError),
}

fn assemble(instruction: Instruction) -> Result<CodeChunk, AssembleError> {
    match instruction {
        Instruction::Machine(_opcode, _operands) => Ok(CodeChunk { code: vec![1] }),
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
