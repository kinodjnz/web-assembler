use crate::assembler::CodeChunk;

#[derive(Debug)]
pub enum PVFlag {
    Overflow(u8), // bit 7
    Parity(u8),   // before parity calculation
}

impl Default for PVFlag {
    fn default() -> PVFlag {
        PVFlag::Overflow(0)
    }
}

impl PVFlag {
    fn as_bit(&self) -> u8 {
        match self {
            PVFlag::Overflow(x) => (x >> 7) & 0x01u8,
            PVFlag::Parity(x) => {
                let x = x ^ (x >> 4);
                let x = x ^ (x >> 2);
                x ^ (x >> 1)
            }
        }
    }
}

#[derive(Debug)]
pub enum NFlag {
    Add,
    Sub,
}

impl Default for NFlag {
    fn default() -> NFlag {
        NFlag::Add
    }
}

impl NFlag {
    fn as_bit(&self) -> u8 {
        match self {
            NFlag::Add => 0,
            NFlag::Sub => 1,
        }
    }
}

#[derive(Debug, Default)]
pub struct Flag {
    sz: u8,  // bit 7, 6
    f53: u8, // bit 5, 3
    h: u8,   // bit 4
    pv: PVFlag,
    n: NFlag,
    cf: u8, // bit 0
}

impl Flag {
    fn as_u8(&self) -> u8 {
        (self.sz & 0xc0)
            | (self.f53 & 0x28)
            | (self.h & 0x10)
            | (self.pv.as_bit() << 2)
            | (self.n.as_bit() << 1)
            | (self.cf & 0x01)
    }
}

#[derive(Debug, Default)]
pub struct Register {
    a: u8,
    f: Flag,
    bc: u16,
    de: u16,
    hl: u16,
    ix: u16,
    iy: u16,
    sp: u16,
    pc: u16,
    i: u8,
    r: u8,
    af_p: u16,
    bc_p: u16,
    de_p: u16,
    hl_p: u16,
}

impl Register {
    fn reset(&mut self) {
        *self = Self::default();
    }

    fn high(rr: u16) -> u8 {
        (rr >> 8) as u8
    }

    fn low(rr: u16) -> u8 {
        rr as u8
    }

    fn b(&self) -> u8 {
        Self::high(self.bc)
    }

    fn c(&self) -> u8 {
        Self::low(self.bc)
    }

    fn d(&self) -> u8 {
        Self::high(self.de)
    }

    fn e(&self) -> u8 {
        Self::low(self.de)
    }

    fn h(&self) -> u8 {
        Self::high(self.hl)
    }

    fn l(&self) -> u8 {
        Self::low(self.hl)
    }

    fn reg8(&self, index: u8) -> u8 {
        match index {
            0 => self.b(),
            1 => self.c(),
            2 => self.d(),
            3 => self.e(),
            4 => self.h(),
            5 => self.l(),
            7 => self.a,
            i => panic!("unknown register: {}", i),
        }
    }

    fn reg16(&self, index: u8) -> u16 {
        match index {
            0 => self.bc,
            1 => self.de,
            2 => self.hl,
            3 => self.sp,
            i => panic!("unknown register: {}", i),
        }
    }

    fn set_reg8(&mut self, index: u8, value: u8) {
        match index {
            0 => self.bc = (self.bc & 0x00ff) | ((value as u16) << 8),
            1 => self.bc = (self.bc & 0xff00) | value as u16,
            2 => self.de = (self.de & 0x00ff) | ((value as u16) << 8),
            3 => self.de = (self.de & 0xff00) | value as u16,
            4 => self.hl = (self.hl & 0x00ff) | ((value as u16) << 8),
            5 => self.hl = (self.hl & 0xff00) | value as u16,
            7 => self.a = value,
            i => panic!("unknown register: {}", i),
        };
    }

    fn set_reg16(&mut self, index: u8, value: u16) {
        match index {
            0 => self.bc = value,
            1 => self.de = value,
            2 => self.hl = value,
            3 => self.sp = value,
            i => panic!("unknown register: {}", i),
        };
    }
}

#[derive(Debug)]
pub struct Emulator {
    mem: Vec<u8>,
    reg: Register,
}

#[derive(Debug)]
pub enum Step {
    Halt,
    IllegalInstruction,
    Run(i32),
}

impl Emulator {
    const MEM_SIZE: usize = 0x10000;

    pub fn new() -> Self {
        let mut e = Emulator {
            mem: Vec::with_capacity(Self::MEM_SIZE),
            reg: Default::default(),
        };
        e.reset();
        e
    }

    pub fn reset(&mut self) {
        self.mem.clear();
        self.mem.resize(Self::MEM_SIZE, 0x00);
        self.reg.reset();
        self.reg.pc = 0x0100;
        self.reg.sp = 0xfffe;
    }

    pub fn load(&mut self, code: &CodeChunk, addr: usize) {
        self.mem[addr..(addr + code.len())].copy_from_slice(&code.code);
    }

    pub fn show_reg(&self) -> String {
        format!("{:?}", self.reg)
    }

    pub fn mem_ref8(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }

    pub fn mem_ref16(&self, addr: u16) -> u16 {
        self.mem_ref8(addr) as u16 | ((self.mem_ref8(addr + 1) as u16) << 8)
    }

    pub fn mem_store8(&mut self, addr: u16, value: u8) {
        self.mem[addr as usize] = value;
    }

    pub fn mem_store16(&mut self, addr: u16, value: u16) {
        self.mem_store8(addr, value as u8);
        self.mem_store8(addr + 1, (value >> 8) as u8);
    }

    fn affect_flag_add8(&mut self, opr1: u8, opr2: u8, res: u32) {
        let resl = res as u8;
        self.reg.f.cf = (res >> 8) as u8;
        self.reg.f.sz = resl;
        self.reg.f.f53 = resl;
        self.reg.f.h = opr1 ^ opr2 ^ resl;
        self.reg.f.pv = PVFlag::Overflow(self.reg.f.h ^ (self.reg.f.cf << 7));
        self.reg.f.n = NFlag::Add;
    }

    // fn affect_flag_sub8(&mut self, opr1: u8, opr2: u8, res: u32) {
    //     let resl = res as u8;
    //     self.reg.f.cf = (res >> 8) as u8;
    //     self.reg.f.sz = resl;
    //     self.reg.f.f53 = resl;
    //     self.reg.f.h = opr1 ^ opr2 ^ resl;
    //     self.reg.f.pv = PVFlag::Overflow(self.f.h ^ (self.f.cf << 7));
    //     self.reg.f.n = NFlag::Sub;
    // }

    pub fn step(&mut self) -> Step {
        match self.mem_ref8(self.reg.pc) {
            0x00 => {
                // nop
                self.reg.pc += 1;
                Step::Run(4)
            }
            op if op & 0xcf == 0x01 => {
                // ld rr,nn
                self.reg.pc += 1;
                let nn = self.mem_ref16(self.reg.pc);
                self.reg.pc += 2;
                self.reg.set_reg16((op >> 4) & 0x03, nn);
                Step::Run(10)
            }
            0x02 => {
                // ld (bc),a
                self.reg.pc += 1;
                self.mem_store8(self.reg.bc, self.reg.a);
                Step::Run(7)
            }
            op if op & 0xcf == 0x03 => {
                // inc rr
                self.reg.pc += 1;
                let index = (op >> 4) & 0x03;
                self.reg.set_reg16(index, self.reg.reg16(index) + 1);
                Step::Run(6)
            }
            0x34 => {
                // inc (hl)
                self.reg.pc += 1;
                self.mem_store8(self.reg.hl, self.mem_ref8(self.reg.hl) + 1);
                Step::Run(11)
            }
            op if op & 0xc7 == 0x04 => {
                // inc r
                self.reg.pc += 1;
                let index = (op >> 3) & 0x07;
                self.reg.set_reg8(index, self.reg.reg8(index) + 1);
                Step::Run(4)
            }
            op if op & 0xc7 == 0x06 => {
                // ld a,n
                self.reg.pc += 1;
                let n = self.mem_ref8(self.reg.pc);
                self.reg.set_reg8((op >> 3) & 0x07, n);
                self.reg.pc += 1;
                Step::Run(7)
            }
            0x76 => Step::Halt,
            op if op & 0xf8 == 0x70 => Step::Halt, // TODO ld (hl),r
            op if op & 0xc7 == 0x46 => Step::Halt, // TODO ld r,(hl)
            op if op & 0xc0 == 0x40 => {
                // ld r,r'
                self.reg.pc += 1;
                let src = (op >> 3) & 0x07;
                self.reg.set_reg8(op & 0x07, self.reg.reg8(src));
                Step::Run(4)
            }
            0x86 => Step::Halt, // TODO add a,(hl)
            op if op & 0xc0 == 0x80 => {
                // add a,r
                self.reg.pc += 1;
                let opr = self.reg.reg8(op & 0x07);
                let res = self.reg.a as u32 + opr as u32;
                self.affect_flag_add8(self.reg.a, opr, res);
                self.reg.a = res as u8;
                Step::Run(4)
            }
            _ => Step::IllegalInstruction,
        }
    }
}
