use crate::assembler::CodeChunk;

#[derive(Debug)]
pub enum ZFlag {
    Acc(u8),
    Bit,
}

impl Default for ZFlag {
    fn default() -> ZFlag {
        ZFlag::Bit
    }
}

impl ZFlag {
    fn as_bit(&self, f: u8) -> u8 {
        match self {
            ZFlag::Acc(x) => {
                if *x == 0 {
                    f | 0x40u8
                } else {
                    f & !0x40u8
                }
            }
            ZFlag::Bit => f,
        }
    }
}

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
            PVFlag::Parity(x) => x.count_ones() as u8 & 0x01u8,
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
            NFlag::Add => 0x00u8,
            NFlag::Sub => 0x02u8,
        }
    }
}

#[derive(Debug, Default)]
pub struct Flag {
    f: u8, // bit 7,(6),5,4,3,1,0
    zf: ZFlag,
    pv: PVFlag,
}

impl Flag {
    const S_MASK: u8 = 0x80u8;
    const Z_MASK: u8 = 0x40u8;
    const F5_MASK: u8 = 0x20u8;
    const H_MASK: u8 = 0x10u8;
    const F3_MASK: u8 = 0x08u8;
    const PV_MASK: u8 = 0x04u8;
    const N_MASK: u8 = 0x02u8;
    const C_MASK: u8 = 0x01u8;
    const F53_MASK: u8 = Flag::F5_MASK | Flag::F3_MASK;

    fn set_wo_z(&mut self, mask: u8, value: u8) {
        self.f = (self.f & !mask) | (value & mask);
    }

    fn set_w_z(&mut self, mask: u8, value: u8) {
        self.zf = ZFlag::Bit;
        self.f = (self.f & !mask) | (value & mask);
    }

    fn as_u8(&self) -> u8 {
        self.zf.as_bit(self.f) | (self.pv.as_bit() << 2)
    }

    fn from_u8(&mut self, x: u8) {
        self.zf = ZFlag::Bit;
        self.f = x;
        self.pv = PVFlag::Overflow(x << 5);
    }

    fn c_bit(&self) -> u8 {
        self.f & Flag::C_MASK
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

trait R16 {
    fn from_pair(h: u8, l: u8) -> Self;
    fn high(&self) -> u8;
    fn low(&self) -> u8;
}

impl R16 for u16 {
    fn from_pair(h: u8, l: u8) -> Self {
        (h as u16) << 8 | l as u16
    }

    fn high(&self) -> u8 {
        (*self >> 8) as u8
    }

    fn low(&self) -> u8 {
        *self as u8
    }
}

impl Register {
    fn reset(&mut self) {
        *self = Self::default();
    }

    fn b(&self) -> u8 {
        self.bc.high()
    }

    fn c(&self) -> u8 {
        self.bc.low()
    }

    fn d(&self) -> u8 {
        self.de.high()
    }

    fn e(&self) -> u8 {
        self.de.low()
    }

    fn h(&self) -> u8 {
        self.hl.high()
    }

    fn l(&self) -> u8 {
        self.hl.low()
    }

    fn set_b(&mut self, x: u8) {
        self.bc = (self.bc & 0x00ff) | ((x as u16) << 8);
    }

    fn set_c(&mut self, x: u8) {
        self.bc = (self.bc & 0xff00) | x as u16
    }

    fn set_d(&mut self, x: u8) {
        self.de = (self.de & 0x00ff) | ((x as u16) << 8);
    }

    fn set_e(&mut self, x: u8) {
        self.de = (self.de & 0xff00) | x as u16;
    }

    fn set_h(&mut self, x: u8) {
        self.hl = (self.hl & 0x00ff) | ((x as u16) << 8);
    }

    fn set_l(&mut self, x: u8) {
        self.hl = (self.hl & 0xff00) | x as u16;
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
            0 => self.set_b(value),
            1 => self.set_c(value),
            2 => self.set_d(value),
            3 => self.set_e(value),
            4 => self.set_h(value),
            5 => self.set_l(value),
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

    fn add_pc(&mut self, i: u16) {
        self.pc = self.pc.wrapping_add(i);
    }

    fn add8_pc(&mut self, i: u8) {
        self.pc = self.pc.wrapping_add(i as i8 as u16);
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
        let cf = self.affect_flag_add_sub8(opr1, opr2, res, NFlag::Add);
        self.reg.f.set_wo_z(Flag::C_MASK, cf);
    }

    fn affect_flag_inc8(&mut self, opr: u8, res: u32) {
        self.affect_flag_add_sub8(opr, 1, res, NFlag::Add);
    }

    fn affect_flag_dec8(&mut self, opr: u8, res: u32) {
        self.affect_flag_add_sub8(opr, 1, res, NFlag::Sub);
    }

    fn affect_flag_add_sub8(&mut self, opr1: u8, opr2: u8, res: u32, n: NFlag) -> u8 {
        let resl = res as u8;
        let cf = (res >> 8) as u8;
        let h = opr1 ^ opr2 ^ resl;
        self.reg.f.set_wo_z(Flag::S_MASK | Flag::F53_MASK, resl);
        self.reg.f.set_wo_z(Flag::H_MASK, h);
        self.reg.f.set_wo_z(Flag::N_MASK, n.as_bit());
        self.reg.f.zf = ZFlag::Acc(resl);
        self.reg.f.pv = PVFlag::Overflow(h ^ (cf << 7));
        cf
    }

    fn affect_flag_add16(&mut self, opr1: u16, opr2: u16, res: u32) {
        let resh = (res >> 8) as u8;
        let cf = (res >> 16) as u8;
        let h = (opr1 >> 8) as u8 ^ (opr2 >> 8) as u8 ^ resh;
        self.reg.f.set_wo_z(Flag::F53_MASK, resh);
        self.reg.f.set_wo_z(Flag::H_MASK, h);
        self.reg
            .f
            .set_wo_z(Flag::N_MASK | Flag::C_MASK, NFlag::Add.as_bit() | cf);
    }

    fn affect_flag_rotate_a(&mut self, res: u8, cf: u8) {
        self.reg.f.set_wo_z(Flag::F53_MASK, res);
        self.reg.f.set_wo_z(
            Flag::H_MASK | Flag::N_MASK | Flag::C_MASK,
            cf & Flag::C_MASK,
        );
        self.reg.f.pv = PVFlag::Parity(res);
    }

    pub fn step(&mut self) -> Step {
        match self.mem_ref8(self.reg.pc) {
            0x00 => {
                // nop
                self.reg.add_pc(1);
                Step::Run(4)
            }
            op if op & 0xcf == 0x01 => {
                // ld rr,nn
                self.reg.add_pc(1);
                let nn = self.mem_ref16(self.reg.pc);
                self.reg.add_pc(2);
                self.reg.set_reg16((op >> 4) & 0x03, nn);
                Step::Run(10)
            }
            0x02 => {
                // ld (bc),a
                self.reg.add_pc(1);
                self.mem_store8(self.reg.bc, self.reg.a);
                Step::Run(7)
            }
            op if op & 0xcf == 0x03 => {
                // inc rr
                self.reg.add_pc(1);
                let index = (op >> 4) & 0x03;
                self.reg
                    .set_reg16(index, self.reg.reg16(index).wrapping_add(1));
                Step::Run(6)
            }
            0x34 => {
                // inc (hl)
                self.reg.add_pc(1);
                let opr = self.mem_ref8(self.reg.hl);
                let res = opr as u32 + 1;
                self.affect_flag_inc8(opr, res);
                self.mem_store8(self.reg.hl, res as u8);
                Step::Run(11)
            }
            op if op & 0xc7 == 0x04 => {
                // inc r
                self.reg.add_pc(1);
                let index = (op >> 3) & 0x07;
                let opr = self.reg.reg8(index);
                let res = opr as u32 + 1;
                self.affect_flag_inc8(opr, res);
                self.reg.set_reg8(index, res as u8);
                Step::Run(4)
            }
            0x35 => {
                // dec (hl)
                self.reg.add_pc(1);
                let opr = self.mem_ref8(self.reg.hl);
                let res = (opr as u32).wrapping_sub(1);
                self.affect_flag_dec8(opr, res);
                self.mem_store8(self.reg.hl, res as u8);
                Step::Run(11)
            }
            op if op & 0xc7 == 0x05 => {
                // dec r
                self.reg.add_pc(1);
                let index = (op >> 3) & 0x07;
                let opr = self.reg.reg8(index);
                let res = (opr as u32).wrapping_sub(1);
                self.affect_flag_dec8(opr, res);
                self.reg.set_reg8(index, res as u8);
                Step::Run(4)
            }
            0x36 => {
                // ld (hl),n
                self.reg.add_pc(1);
                let n = self.mem_ref8(self.reg.pc);
                self.mem_store8(self.reg.hl, n);
                self.reg.add_pc(1);
                Step::Run(10)
            }
            op if op & 0xc7 == 0x06 => {
                // ld r,n
                self.reg.add_pc(1);
                let n = self.mem_ref8(self.reg.pc);
                self.reg.set_reg8((op >> 3) & 0x07, n);
                self.reg.add_pc(1);
                Step::Run(7)
            }
            0x07 => {
                // rlca
                self.reg.add_pc(1);
                let res = (self.reg.a << 1) | (self.reg.a >> 7);
                self.affect_flag_rotate_a(res, self.reg.a >> 7);
                self.reg.a = res;
                Step::Run(4)
            }
            0x08 => {
                // ex af,af'
                self.reg.add_pc(1);
                let a = self.reg.a;
                let f = self.reg.f.as_u8();
                self.reg.a = self.reg.af_p.high();
                self.reg.f.from_u8(self.reg.af_p.low());
                self.reg.af_p = u16::from_pair(a, f);
                Step::Run(4)
            }
            op if op & 0xcf == 0x09 => {
                // add hl,rr
                self.reg.add_pc(1);
                let opr = self.reg.reg16((op >> 4) & 0x03);
                let res = self.reg.hl as u32 + opr as u32;
                self.affect_flag_add16(self.reg.hl, opr, res);
                self.reg.hl = res as u16;
                Step::Run(11)
            }
            0x0a => {
                // ld a,(bc)
                self.reg.add_pc(1);
                self.reg.a = self.mem_ref8(self.reg.bc);
                Step::Run(7)
            }
            op if op & 0xcf == 0x0b => {
                // dec rr
                self.reg.add_pc(1);
                let index = (op >> 4) & 0x03;
                self.reg
                    .set_reg16(index, self.reg.reg16(index).wrapping_sub(1));
                Step::Run(6)
            }
            0x0f => {
                // rrca
                self.reg.add_pc(1);
                let res = (self.reg.a >> 1) | (self.reg.a << 7);
                self.affect_flag_rotate_a(res, self.reg.a);
                self.reg.a = res;
                Step::Run(4)
            }
            0x10 => {
                // djnz
                self.reg.add_pc(1);
                let o = self.mem_ref8(self.reg.pc);
                self.reg.add_pc(1);
                let x = self.reg.b().wrapping_sub(1);
                self.reg.set_b(x);
                if x == 0x00u8 {
                    Step::Run(8)
                } else {
                    self.reg.add8_pc(o);
                    Step::Run(13)
                }
            }
            0x17 => {
                // rla
                self.reg.add_pc(1);
                let res = (self.reg.a << 1) | self.reg.f.c_bit();
                self.affect_flag_rotate_a(res, self.reg.a >> 7);
                self.reg.a = res;
                Step::Run(4)
            }
            0x76 => Step::Halt,
            op if op & 0xf8 == 0x70 => Step::Halt, // TODO ld (hl),r
            op if op & 0xc7 == 0x46 => Step::Halt, // TODO ld r,(hl)
            op if op & 0xc0 == 0x40 => {
                // ld r,r'
                self.reg.add_pc(1);
                let src = (op >> 3) & 0x07;
                self.reg.set_reg8(op & 0x07, self.reg.reg8(src));
                Step::Run(4)
            }
            0x86 => Step::Halt, // TODO add a,(hl)
            op if op & 0xc0 == 0x80 => {
                // add a,r
                self.reg.add_pc(1);
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
