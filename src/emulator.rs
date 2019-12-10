use crate::assembler::CodeChunk;

#[derive(Debug, Clone, Copy)]
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
    fn as_bit(self, f: u8) -> u8 {
        match self {
            ZFlag::Acc(x) => {
                if x == 0 {
                    f | 0x40u8
                } else {
                    f & !0x40u8
                }
            }
            ZFlag::Bit => f,
        }
    }
}

#[derive(Debug, Clone, Copy)]
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
    fn as_bit(self) -> u8 {
        match self {
            PVFlag::Overflow(x) => (x >> 7) & 0x01u8,
            PVFlag::Parity(x) => (x.count_ones() + 1) as u8 & 0x01u8,
        }
    }
}

#[derive(Debug, Clone, Copy)]
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
    fn as_bit(self) -> u8 {
        match self {
            NFlag::Add => 0x00u8,
            NFlag::Sub => 0x02u8,
        }
    }
}

#[derive(Debug, Default, Clone)]
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

    pub fn as_u8(&self) -> u8 {
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

    fn is_c(&self) -> bool {
        self.f & Flag::C_MASK != 0
    }

    fn is_h(&self) -> bool {
        self.f & Flag::H_MASK != 0
    }

    fn is_n(&self) -> bool {
        self.f & Flag::N_MASK != 0
    }

    fn cond(&self, index: u8) -> bool {
        match index {
            0 => self.zf.as_bit(self.f) & Flag::Z_MASK == 0,
            1 => self.zf.as_bit(self.f) & Flag::Z_MASK != 0,
            2 => !self.is_c(),
            3 => self.is_c(),
            4 => self.pv.as_bit() == 0,
            5 => self.pv.as_bit() != 0,
            6 => self.f & Flag::S_MASK == 0,
            7 => self.f & Flag::S_MASK != 0,
            i => panic!("unknown condition: {}", i),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Register {
    pub a: u8,
    pub f: Flag,
    pub bc: u16,
    pub de: u16,
    pub hl: u16,
    pub ix: u16,
    pub iy: u16,
    pub sp: u16,
    pub pc: u16,
    pub i: u8,
    pub r: u8,
    pub af_p: u16,
    pub bc_p: u16,
    pub de_p: u16,
    pub hl_p: u16,
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
    pub mem: Vec<u8>,
    pub reg: Register,
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

    fn affect_flag_sub8(&mut self, opr1: u8, opr2: u8, res: u32) {
        let cf = self.affect_flag_add_sub8(opr1, opr2, res, NFlag::Sub);
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

    fn affect_flag_and(&mut self, res: u8) {
        self.reg.f.set_wo_z(Flag::S_MASK | Flag::F53_MASK, res);
        self.reg
            .f
            .set_wo_z(Flag::H_MASK | Flag::N_MASK | Flag::C_MASK, Flag::H_MASK);
        self.reg.f.zf = ZFlag::Acc(res);
        self.reg.f.pv = PVFlag::Parity(res);
    }

    fn affect_flag_or_xor(&mut self, res: u8) {
        self.reg.f.set_wo_z(
            Flag::S_MASK | Flag::F53_MASK | Flag::H_MASK | Flag::N_MASK | Flag::C_MASK,
            res & (Flag::S_MASK | Flag::F53_MASK),
        );
        self.reg.f.zf = ZFlag::Acc(res);
        self.reg.f.pv = PVFlag::Parity(res);
    }

    fn affect_flag_cp(&mut self, opr1: u8, opr2: u8, res: u32) {
        let resl = res as u8;
        let cf = (res >> 8) as u8;
        let h = opr1 ^ opr2 ^ resl;
        self.reg.f.set_wo_z(Flag::S_MASK, resl);
        self.reg.f.set_wo_z(Flag::F53_MASK, opr2);
        self.reg.f.set_wo_z(Flag::H_MASK, h);
        self.reg.f.set_wo_z(Flag::N_MASK, NFlag::Sub.as_bit());
        self.reg.f.set_wo_z(Flag::C_MASK, cf);
        self.reg.f.zf = ZFlag::Acc(resl);
        self.reg.f.pv = PVFlag::Overflow(h ^ (cf << 7));
    }

    fn op_nop(&mut self, _: u8) -> Step {
        Step::Run(4)
    }

    fn op_ld_rr_nn(&mut self, op: u8) -> Step {
        let nn = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.reg.set_reg16((op >> 4) & 0x03, nn);
        Step::Run(10)
    }

    fn op_ld_ind_bc_a(&mut self, _: u8) -> Step {
        self.mem_store8(self.reg.bc, self.reg.a);
        Step::Run(7)
    }

    fn op_inc_rr(&mut self, op: u8) -> Step {
        let index = (op >> 4) & 0x03;
        self.reg
            .set_reg16(index, self.reg.reg16(index).wrapping_add(1));
        Step::Run(6)
    }

    fn op_inc_ind_hl(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.hl);
        let res = opr as u32 + 1;
        self.affect_flag_inc8(opr, res);
        self.mem_store8(self.reg.hl, res as u8);
        Step::Run(11)
    }

    fn op_inc_r(&mut self, op: u8) -> Step {
        let index = (op >> 3) & 0x07;
        let opr = self.reg.reg8(index);
        let res = opr as u32 + 1;
        self.affect_flag_inc8(opr, res);
        self.reg.set_reg8(index, res as u8);
        Step::Run(4)
    }

    fn op_dec_ind_hl(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.hl);
        let res = (opr as u32).wrapping_sub(1);
        self.affect_flag_dec8(opr, res);
        self.mem_store8(self.reg.hl, res as u8);
        Step::Run(11)
    }

    fn op_dec_r(&mut self, op: u8) -> Step {
        let index = (op >> 3) & 0x07;
        let opr = self.reg.reg8(index);
        let res = (opr as u32).wrapping_sub(1);
        self.affect_flag_dec8(opr, res);
        self.reg.set_reg8(index, res as u8);
        Step::Run(4)
    }

    fn op_ld_ind_hl_n(&mut self, _: u8) -> Step {
        let n = self.mem_ref8(self.reg.pc);
        self.mem_store8(self.reg.hl, n);
        self.reg.add_pc(1);
        Step::Run(10)
    }

    fn op_ld_r_n(&mut self, op: u8) -> Step {
        let n = self.mem_ref8(self.reg.pc);
        self.reg.set_reg8((op >> 3) & 0x07, n);
        self.reg.add_pc(1);
        Step::Run(7)
    }

    fn op_rlca(&mut self, _: u8) -> Step {
        let res = (self.reg.a << 1) | (self.reg.a >> 7);
        self.affect_flag_rotate_a(res, self.reg.a >> 7);
        self.reg.a = res;
        Step::Run(4)
    }

    fn op_ex_af_af_p(&mut self, _: u8) -> Step {
        let a = self.reg.a;
        let f = self.reg.f.as_u8();
        self.reg.a = self.reg.af_p.high();
        self.reg.f.from_u8(self.reg.af_p.low());
        self.reg.af_p = u16::from_pair(a, f);
        Step::Run(4)
    }

    fn op_add_hl_rr(&mut self, op: u8) -> Step {
        let opr = self.reg.reg16((op >> 4) & 0x03);
        let res = self.reg.hl as u32 + opr as u32;
        self.affect_flag_add16(self.reg.hl, opr, res);
        self.reg.hl = res as u16;
        Step::Run(11)
    }

    fn op_ld_a_ind_bc(&mut self, _: u8) -> Step {
        self.reg.a = self.mem_ref8(self.reg.bc);
        Step::Run(7)
    }

    fn op_dec_rr(&mut self, op: u8) -> Step {
        let index = (op >> 4) & 0x03;
        self.reg
            .set_reg16(index, self.reg.reg16(index).wrapping_sub(1));
        Step::Run(6)
    }

    fn op_rrca(&mut self, _: u8) -> Step {
        let res = (self.reg.a >> 1) | (self.reg.a << 7);
        self.affect_flag_rotate_a(res, self.reg.a);
        self.reg.a = res;
        Step::Run(4)
    }

    fn op_djnz_o(&mut self, _: u8) -> Step {
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

    fn op_ld_ind_de_a(&mut self, _: u8) -> Step {
        self.mem_store8(self.reg.de, self.reg.a);
        Step::Run(7)
    }

    fn op_rla(&mut self, _: u8) -> Step {
        let res = (self.reg.a << 1) | self.reg.f.c_bit();
        self.affect_flag_rotate_a(res, self.reg.a >> 7);
        self.reg.a = res;
        Step::Run(4)
    }

    fn op_jr_o(&mut self, _: u8) -> Step {
        let o = self.mem_ref8(self.reg.pc);
        self.reg.add_pc(1);
        self.reg.add8_pc(o);
        Step::Run(12)
    }

    fn op_ld_a_ind_de(&mut self, _: u8) -> Step {
        self.reg.a = self.mem_ref8(self.reg.de);
        Step::Run(7)
    }

    fn op_rra(&mut self, _: u8) -> Step {
        let res = (self.reg.a >> 1) | (self.reg.f.c_bit() << 7);
        self.affect_flag_rotate_a(res, self.reg.a);
        self.reg.a = res;
        Step::Run(4)
    }

    fn op_jr_cond_o(&mut self, op: u8) -> Step {
        let o = self.mem_ref8(self.reg.pc);
        self.reg.add_pc(1);
        if self.reg.f.cond((op >> 3) & 3) {
            self.reg.add8_pc(o);
            Step::Run(12)
        } else {
            Step::Run(7)
        }
    }

    fn op_ld_ind_nn_hl(&mut self, _: u8) -> Step {
        let nn = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.mem_store16(nn, self.reg.hl);
        Step::Run(16)
    }

    fn op_daa(&mut self, _: u8) -> Step {
        let c = self.reg.f.is_c() || self.reg.a > 0x99;
        let mut offset = 0x00u8;
        if (self.reg.a & 0x0f) > 0x09 || self.reg.f.is_h() {
            offset += 0x06u8;
        }
        if c || self.reg.a > 0x99 {
            offset += 0x60u8;
        }
        let h = match (self.reg.f.is_n(), self.reg.f.is_h()) {
            (true, false) => false,
            (true, true) => (self.reg.a & 0x0f) < 0x06,
            _ => (self.reg.a & 0x0f) > 0x09,
        };
        let a = if self.reg.f.is_n() {
            self.reg.a.wrapping_sub(offset)
        } else {
            self.reg.a.wrapping_add(offset)
        };
        self.reg.a = a;
        self.reg.f.set_wo_z(Flag::S_MASK | Flag::F53_MASK, a);
        self.reg
            .f
            .set_wo_z(Flag::H_MASK | Flag::C_MASK, ((h as u8) << 4) | c as u8);
        self.reg.f.zf = ZFlag::Acc(a);
        self.reg.f.pv = PVFlag::Parity(a);
        Step::Run(4)
    }

    fn op_ld_hl_ind_nn(&mut self, _: u8) -> Step {
        let nn = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.reg.hl = self.mem_ref16(nn);
        Step::Run(16)
    }

    fn op_cpl(&mut self, _: u8) -> Step {
        self.reg.a = !self.reg.a;
        self.reg.f.set_wo_z(Flag::F53_MASK, self.reg.a);
        self.reg
            .f
            .set_wo_z(Flag::H_MASK | Flag::N_MASK, Flag::H_MASK | Flag::N_MASK);
        Step::Run(4)
    }

    fn op_ld_ind_nn_a(&mut self, _: u8) -> Step {
        let nn = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.mem_store8(nn, self.reg.a);
        Step::Run(13)
    }

    fn op_scf(&mut self, _: u8) -> Step {
        self.reg.f.set_wo_z(Flag::F53_MASK, self.reg.a);
        self.reg
            .f
            .set_wo_z(Flag::H_MASK | Flag::N_MASK | Flag::C_MASK, Flag::C_MASK);
        Step::Run(4)
    }

    fn op_ld_a_ind_nn(&mut self, _: u8) -> Step {
        let nn = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.reg.a = self.mem_ref8(nn);
        Step::Run(13)
    }

    fn op_ccf(&mut self, _: u8) -> Step {
        self.reg.f.set_wo_z(Flag::F53_MASK, self.reg.a);
        let c = self.reg.f.is_c() as u8;
        self.reg.f.set_wo_z(
            Flag::H_MASK | Flag::N_MASK | Flag::C_MASK,
            (c ^ 0x01) | (c << 4),
        );
        Step::Run(4)
    }

    fn op_ld_ind_hl_r(&mut self, op: u8) -> Step {
        self.mem_store8(self.reg.hl, self.reg.reg8(op & 0x07));
        Step::Run(7)
    }

    fn op_ld_r_ind_hl(&mut self, op: u8) -> Step {
        self.reg
            .set_reg8((op >> 3) & 0x07, self.mem_ref8(self.reg.hl));
        Step::Run(7)
    }

    fn op_ld_r_r(&mut self, op: u8) -> Step {
        let src = (op >> 3) & 0x07;
        self.reg.set_reg8(op & 0x07, self.reg.reg8(src));
        Step::Run(4)
    }

    fn op_add_a_ind_hl(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.hl);
        let res = self.reg.a as u32 + opr as u32;
        self.affect_flag_add8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(7)
    }

    fn op_add_a_r(&mut self, op: u8) -> Step {
        let opr = self.reg.reg8(op & 0x07);
        let res = self.reg.a as u32 + opr as u32;
        self.affect_flag_add8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(4)
    }

    fn op_adc_a_ind_hl(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.hl);
        let res = self.reg.a as u32 + opr as u32 + self.reg.f.c_bit() as u32;
        self.affect_flag_add8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(7)
    }

    fn op_adc_a_r(&mut self, op: u8) -> Step {
        let opr = self.reg.reg8(op & 0x07);
        let res = self.reg.a as u32 + opr as u32 + self.reg.f.c_bit() as u32;
        self.affect_flag_add8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(4)
    }

    fn op_sub_ind_hl(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.hl);
        let res = (self.reg.a as u32).wrapping_sub(opr as u32);
        self.affect_flag_sub8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(7)
    }

    fn op_sub_r(&mut self, op: u8) -> Step {
        let opr = self.reg.reg8(op & 0x07);
        let res = (self.reg.a as u32).wrapping_sub(opr as u32);
        self.affect_flag_sub8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(4)
    }

    fn op_sbc_a_ind_hl(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.hl);
        let res = (self.reg.a as u32)
            .wrapping_sub(opr as u32)
            .wrapping_sub(self.reg.f.c_bit() as u32);
        self.affect_flag_sub8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(7)
    }

    fn op_sbc_a_r(&mut self, op: u8) -> Step {
        let opr = self.reg.reg8(op & 0x07);
        let res = (self.reg.a as u32)
            .wrapping_sub(opr as u32)
            .wrapping_sub(self.reg.f.c_bit() as u32);
        self.affect_flag_sub8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(4)
    }

    fn op_and_ind_hl(&mut self, _: u8) -> Step {
        let res = self.reg.a & self.mem_ref8(self.reg.hl);
        self.affect_flag_and(res);
        self.reg.a = res;
        Step::Run(7)
    }

    fn op_and_r(&mut self, op: u8) -> Step {
        let res = self.reg.a & self.reg.reg8(op & 0x07);
        self.affect_flag_and(res);
        self.reg.a = res;
        Step::Run(4)
    }

    fn op_xor_ind_hl(&mut self, _: u8) -> Step {
        let res = self.reg.a ^ self.mem_ref8(self.reg.hl);
        self.affect_flag_or_xor(res);
        self.reg.a = res;
        Step::Run(7)
    }

    fn op_xor_r(&mut self, op: u8) -> Step {
        let res = self.reg.a ^ self.reg.reg8(op & 0x07);
        self.affect_flag_or_xor(res);
        self.reg.a = res;
        Step::Run(4)
    }

    fn op_or_ind_hl(&mut self, _: u8) -> Step {
        let res = self.reg.a | self.mem_ref8(self.reg.hl);
        self.affect_flag_or_xor(res);
        self.reg.a = res;
        Step::Run(7)
    }

    fn op_or_r(&mut self, op: u8) -> Step {
        let res = self.reg.a | self.reg.reg8(op & 0x07);
        self.affect_flag_or_xor(res);
        self.reg.a = res;
        Step::Run(4)
    }

    fn op_cp_ind_hl(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.hl);
        let res = (self.reg.a as u32).wrapping_sub(opr as u32);
        self.affect_flag_cp(self.reg.a, opr, res);
        Step::Run(7)
    }

    fn op_cp_r(&mut self, op: u8) -> Step {
        let opr = self.reg.reg8(op & 0x07);
        let res = (self.reg.a as u32).wrapping_sub(opr as u32);
        self.affect_flag_cp(self.reg.a, opr, res);
        Step::Run(4)
    }

    fn op_ret_cond(&mut self, op: u8) -> Step {
        if self.reg.f.cond((op >> 3) & 0x07) {
            self.reg.sp = self.reg.sp.wrapping_add(2);
            self.reg.pc = self.mem_ref16(self.reg.sp);
            Step::Run(11)
        } else {
            Step::Run(5)
        }
    }

    fn op_pop_rr(&mut self, op: u8) -> Step {
        let value = self.mem_ref16(self.reg.sp);
        self.reg.sp = self.reg.sp.wrapping_add(2);
        match (op >> 4) & 0x03 {
            0 => self.reg.bc = value,
            1 => self.reg.de = value,
            2 => self.reg.hl = value,
            3 => {
                self.reg.a = value.high();
                self.reg.f.from_u8(value.low());
            }
            i => panic!("unknown register: {}", i),
        };
        Step::Run(10)
    }

    fn op_jp_cond_nn(&mut self, op: u8) -> Step {
        if self.reg.f.cond((op >> 3) & 0x07) {
            self.reg.pc = self.mem_ref16(self.reg.pc);
        } else {
            self.reg.add_pc(2);
        }
        Step::Run(10)
    }

    fn op_jp_nn(&mut self, _: u8) -> Step {
        self.reg.pc = self.mem_ref16(self.reg.pc);
        Step::Run(10)
    }

    fn op_call_cond_nn(&mut self, op: u8) -> Step {
        if self.reg.f.cond((op >> 3) & 0x07) {
            let pc = self.mem_ref16(self.reg.pc);
            self.reg.add_pc(2);
            self.mem_store16(self.reg.sp, self.reg.pc);
            self.reg.sp = self.reg.sp.wrapping_sub(2);
            self.reg.pc = pc;
            Step::Run(17)
        } else {
            self.reg.add_pc(2);
            Step::Run(10)
        }
    }

    fn op_push_rr(&mut self, op: u8) -> Step {
        self.reg.sp = self.reg.sp.wrapping_sub(2);
        let value = match (op >> 4) & 0x03 {
            0 => self.reg.bc,
            1 => self.reg.de,
            2 => self.reg.hl,
            3 => R16::from_pair(self.reg.a, self.reg.f.as_u8()),
            i => panic!("unknown register: {}", i),
        };
        self.mem_store16(self.reg.sp, value);
        Step::Run(11)
    }

    fn op_add_a_n(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.pc);
        self.reg.add_pc(1);
        let res = self.reg.a as u32 + opr as u32;
        self.affect_flag_add8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(7)
    }

    fn op_rst_n(&mut self, op: u8) -> Step {
        self.mem_store16(self.reg.sp, self.reg.pc);
        self.reg.sp = self.reg.sp.wrapping_sub(2);
        self.reg.pc = (op & 0x38u8) as u16;
        Step::Run(11)
    }

    fn op_ret(&mut self, _: u8) -> Step {
        self.reg.sp = self.reg.sp.wrapping_add(2);
        self.reg.pc = self.mem_ref16(self.reg.sp);
        Step::Run(10)
    }

    fn op_call_nn(&mut self, _: u8) -> Step {
        let pc = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.mem_store16(self.reg.sp, self.reg.pc);
        self.reg.sp = self.reg.sp.wrapping_sub(2);
        self.reg.pc = pc;
        Step::Run(17)
    }

    fn op_adc_a_n(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.pc);
        self.reg.add_pc(1);
        let res = self.reg.a as u32 + opr as u32 + self.reg.f.c_bit() as u32;
        self.affect_flag_add8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(7)
    }

    fn op_out_ind_n_a(&mut self, _: u8) -> Step {
        // TODO unimplemented
        self.reg.add_pc(1);
        Step::Run(11)
    }

    fn op_sub_n(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.pc);
        self.reg.add_pc(1);
        let res = (self.reg.a as u32).wrapping_sub(opr as u32);
        self.affect_flag_sub8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(7)
    }

    fn op_exx(&mut self, _: u8) -> Step {
        std::mem::swap(&mut self.reg.bc, &mut self.reg.bc_p);
        std::mem::swap(&mut self.reg.de, &mut self.reg.de_p);
        std::mem::swap(&mut self.reg.hl, &mut self.reg.hl_p);
        Step::Run(4)
    }

    fn op_in_a_ind_n(&mut self, _: u8) -> Step {
        // TODO unimplemented
        self.reg.a = 0u8;
        Step::Run(11)
    }

    pub fn step(&mut self) -> Step {
        let op = self.mem_ref8(self.reg.pc);
        let mut run_op = |f: fn(&mut Self, u8) -> Step| {
            self.reg.add_pc(1);
            f(self, op)
        };
        match op {
            0x00 => run_op(Self::op_nop),
            op if op & 0xcf == 0x01 => run_op(Self::op_ld_rr_nn),
            0x02 => run_op(Self::op_ld_ind_bc_a),
            op if op & 0xcf == 0x03 => run_op(Self::op_inc_rr),
            0x34 => run_op(Self::op_inc_ind_hl),
            op if op & 0xc7 == 0x04 => run_op(Self::op_inc_r),
            0x35 => run_op(Self::op_dec_ind_hl),
            op if op & 0xc7 == 0x05 => run_op(Self::op_dec_r),
            0x36 => run_op(Self::op_ld_ind_hl_n),
            op if op & 0xc7 == 0x06 => run_op(Self::op_ld_r_n),
            0x07 => run_op(Self::op_rlca),
            0x08 => run_op(Self::op_ex_af_af_p),
            op if op & 0xcf == 0x09 => run_op(Self::op_add_hl_rr),
            0x0a => run_op(Self::op_ld_a_ind_bc),
            op if op & 0xcf == 0x0b => run_op(Self::op_dec_rr),
            0x0f => run_op(Self::op_rrca),
            0x10 => run_op(Self::op_djnz_o),
            0x12 => run_op(Self::op_ld_ind_de_a),
            0x17 => run_op(Self::op_rla),
            0x18 => run_op(Self::op_jr_o),
            0x1a => run_op(Self::op_ld_a_ind_de),
            0x1f => run_op(Self::op_rra),
            op if op & 0xe7 == 0x20 => run_op(Self::op_jr_cond_o),
            0x22 => run_op(Self::op_ld_ind_nn_hl),
            0x27 => run_op(Self::op_daa),
            0x2a => run_op(Self::op_ld_hl_ind_nn),
            0x2f => run_op(Self::op_cpl),
            0x32 => run_op(Self::op_ld_ind_nn_a),
            0x37 => run_op(Self::op_scf),
            0x3a => run_op(Self::op_ld_a_ind_nn),
            0x3f => run_op(Self::op_ccf),
            0x76 => Step::Halt,
            op if op & 0xf8 == 0x70 => run_op(Self::op_ld_ind_hl_r),
            op if op & 0xc7 == 0x46 => run_op(Self::op_ld_r_ind_hl),
            op if op & 0xc0 == 0x40 => run_op(Self::op_ld_r_r),
            0x86 => run_op(Self::op_add_a_ind_hl),
            op if op & 0xf8 == 0x80 => run_op(Self::op_add_a_r),
            0x8e => run_op(Self::op_adc_a_ind_hl),
            op if op & 0xf8 == 0x88 => run_op(Self::op_adc_a_r),
            0x96 => run_op(Self::op_sub_ind_hl),
            op if op & 0xf8 == 0x90 => run_op(Self::op_sub_r),
            0x9e => run_op(Self::op_sbc_a_ind_hl),
            op if op & 0xf8 == 0x98 => run_op(Self::op_sbc_a_r),
            0xa6 => run_op(Self::op_and_ind_hl),
            op if op & 0xf8 == 0xa0 => run_op(Self::op_and_r),
            0xae => run_op(Self::op_xor_ind_hl),
            op if op & 0xf8 == 0xa8 => run_op(Self::op_xor_r),
            0xb6 => run_op(Self::op_or_ind_hl),
            op if op & 0xf8 == 0xb0 => run_op(Self::op_or_r),
            0xbe => run_op(Self::op_cp_ind_hl),
            op if op & 0xf8 == 0xb8 => run_op(Self::op_cp_r),
            op if op & 0xc7 == 0xc0 => run_op(Self::op_ret_cond),
            op if op & 0xcf == 0xc1 => run_op(Self::op_pop_rr),
            op if op & 0xc7 == 0xc2 => run_op(Self::op_jp_cond_nn),
            0xc3 => run_op(Self::op_jp_nn),
            op if op & 0xc7 == 0xc4 => run_op(Self::op_call_cond_nn),
            op if op & 0xcf == 0xc5 => run_op(Self::op_push_rr),
            0xc6 => run_op(Self::op_add_a_n),
            op if op & 0xc7 == 0xc7 => run_op(Self::op_rst_n),
            0xc9 => run_op(Self::op_ret),
            0xcb => run_op(Self::op_bits),
            0xcd => run_op(Self::op_call_nn),
            0xce => run_op(Self::op_adc_a_n),
            0xd3 => run_op(Self::op_out_ind_n_a),
            0xd6 => run_op(Self::op_sub_n),
            0xd9 => run_op(Self::op_exx),
            0xdb => run_op(Self::op_in_a_ind_n),
            _ => Step::IllegalInstruction,
        }
    }

    fn op_bits(&mut self, _: u8) -> Step {
        // TODO bits instructions
        Step::IllegalInstruction
    }
}
