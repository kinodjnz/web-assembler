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

    fn from_bool(b: bool) -> PVFlag {
        PVFlag::Overflow((b as u8) << 7)
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

    fn ixh(&self) -> u8 {
        self.ix.high()
    }

    fn ixl(&self) -> u8 {
        self.ix.low()
    }

    fn iyh(&self) -> u8 {
        self.iy.high()
    }

    fn iyl(&self) -> u8 {
        self.iy.low()
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

    fn set_ixh(&mut self, x: u8) {
        self.ix = (self.ix & 0x00ff) | ((x as u16) << 8);
    }

    fn set_ixl(&mut self, x: u8) {
        self.ix = (self.ix & 0xff00) | x as u16;
    }

    fn set_iyh(&mut self, x: u8) {
        self.iy = (self.iy & 0x00ff) | ((x as u16) << 8);
    }

    fn set_iyl(&mut self, x: u8) {
        self.iy = (self.iy & 0xff00) | x as u16;
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

    fn reg_ixy8(&self, ixy: IXY, index: u8) -> u8 {
        match (ixy, index) {
            (IXY::IX, 4) => self.ixh(),
            (IXY::IX, 5) => self.ixl(),
            (IXY::IY, 4) => self.iyh(),
            (IXY::IY, 5) => self.iyl(),
            i => panic!("unknown register: {:?}", i),
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

    fn reg_ixy16(&self, ixy: IXY, index: u8) -> u16 {
        match (ixy, index) {
            (_, 0) => self.bc,
            (_, 1) => self.de,
            (IXY::IX, 2) => self.ix,
            (IXY::IY, 2) => self.iy,
            (_, 3) => self.sp,
            i => panic!("unknown register: {:?}", i),
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

    fn set_reg_ixy8(&mut self, ixy: IXY, index: u8, value: u8) {
        match (ixy, index) {
            (IXY::IX, 4) => self.set_ixh(value),
            (IXY::IX, 5) => self.set_ixl(value),
            (IXY::IY, 4) => self.set_iyh(value),
            (IXY::IY, 5) => self.set_iyl(value),
            i => panic!("unknown register: {:?}", i),
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

    fn set_reg_ixy16(&mut self, ixy: IXY, index: u8, value: u16) {
        match (ixy, index) {
            (_, 0) => self.bc = value,
            (_, 1) => self.de = value,
            (IXY::IX, 2) => self.ix = value,
            (IXY::IY, 2) => self.iy = value,
            (_, 3) => self.sp = value,
            i => panic!("unknown register: {:?}", i),
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

#[derive(Debug, Clone, Copy)]
enum IXY {
    IX,
    IY,
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
        self.mem_ref8(addr) as u16 | ((self.mem_ref8(addr.wrapping_add(1)) as u16) << 8)
    }

    pub fn mem_store8(&mut self, addr: u16, value: u8) {
        self.mem[addr as usize] = value;
    }

    pub fn mem_store16(&mut self, addr: u16, value: u16) {
        self.mem_store8(addr, value as u8);
        self.mem_store8(addr + 1, (value >> 8) as u8);
    }

    fn ixy_offset(&mut self, ixy: IXY) -> u16 {
        let offset = self.mem_ref8(self.reg.pc) as i8 as u16;
        self.reg.add_pc(1);
        self.reg.reg_ixy16(ixy, 2).wrapping_add(offset)
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
        self.affect_flag_add_sub16(opr1, opr2, res, NFlag::Add)
    }

    fn affect_flag_sub16(&mut self, opr1: u16, opr2: u16, res: u32) {
        self.affect_flag_add_sub16(opr1, opr2, res, NFlag::Sub)
    }

    fn affect_flag_add_sub16(&mut self, opr1: u16, opr2: u16, res: u32, n: NFlag) {
        let resh = (res >> 8) as u8;
        let cf = (res >> 16) as u8;
        let h = (opr1 >> 8) as u8 ^ (opr2 >> 8) as u8 ^ resh;
        self.reg.f.set_wo_z(Flag::F53_MASK, resh);
        self.reg.f.set_wo_z(Flag::H_MASK, h);
        self.reg
            .f
            .set_wo_z(Flag::N_MASK | Flag::C_MASK, n.as_bit() | cf);
    }

    fn affect_flag_rotate_a(&mut self, res: u8, cf: u8) {
        self.reg.f.set_wo_z(Flag::F53_MASK, res);
        self.reg.f.set_wo_z(
            Flag::H_MASK | Flag::N_MASK | Flag::C_MASK,
            cf & Flag::C_MASK,
        );
    }

    fn affect_flag_rotate_shift(&mut self, res: u8, cf: u8) {
        self.reg.f.set_wo_z(Flag::S_MASK | Flag::F53_MASK, res);
        self.reg.f.set_wo_z(
            Flag::H_MASK | Flag::N_MASK | Flag::C_MASK,
            cf & Flag::C_MASK,
        );
        self.reg.f.zf = ZFlag::Acc(res);
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

    fn affect_flag_in(&mut self, res: u8) {
        self.reg.f.set_wo_z(
            Flag::S_MASK | Flag::F53_MASK | Flag::H_MASK | Flag::N_MASK,
            res & (Flag::S_MASK | Flag::F53_MASK),
        );
        self.reg.f.zf = ZFlag::Acc(res);
        self.reg.f.pv = PVFlag::Parity(res);
    }

    fn affect_flag_ld_i_r(&mut self, res: u8) {
        self.reg.f.set_wo_z(
            Flag::S_MASK | Flag::F53_MASK | Flag::H_MASK | Flag::N_MASK,
            res & (Flag::S_MASK | Flag::F53_MASK),
        );
        self.reg.f.zf = ZFlag::Acc(res);
        self.reg.f.pv = PVFlag::Overflow(0); // TODO copy iff
    }

    fn affect_flag_rotate_decimal(&mut self, a: u8) {
        self.reg.f.set_wo_z(
            Flag::S_MASK | Flag::F53_MASK | Flag::H_MASK | Flag::N_MASK,
            a & (Flag::S_MASK | Flag::F53_MASK),
        );
        self.reg.f.zf = ZFlag::Acc(a);
        self.reg.f.pv = PVFlag::Parity(a);
    }

    fn affect_flag_ldi_ldd(&mut self, a: u8, opr: u8, bc: u16) {
        let n = a.wrapping_add(opr);
        self.reg.f.set_wo_z(
            Flag::F53_MASK | Flag::H_MASK | Flag::N_MASK,
            (n & Flag::F3_MASK) | ((n << 4) & Flag::F5_MASK),
        );
        self.reg.f.pv = PVFlag::from_bool(bc == 0);
    }

    fn affect_flag_cpi_cpd(&mut self, a: u8, opr: u8, bc: u16) {
        let res = (a as u32).wrapping_sub(opr as u32);
        let resl = res as u8;
        let h = a ^ opr ^ resl;
        let n = resl.wrapping_sub((h >> 4) & 0x01u8);
        self.reg.f.set_wo_z(Flag::S_MASK, resl);
        self.reg.f.set_wo_z(
            Flag::F53_MASK,
            (n & Flag::F3_MASK) | ((n << 4) & Flag::F5_MASK),
        );
        self.reg.f.set_wo_z(Flag::H_MASK, h);
        self.reg.f.set_wo_z(Flag::N_MASK, NFlag::Sub.as_bit());
        self.reg.f.zf = ZFlag::Acc(resl);
        self.reg.f.pv = PVFlag::from_bool(bc == 0);
    }

    fn affect_flag_ini_ind(&mut self, _opr: u8, b: u8, _c: u8) {
        self.reg.f.set_wo_z(Flag::F53_MASK | Flag::S_MASK, b);
        self.reg.f.zf = ZFlag::Acc(b);
        // TODO H, N, PV, C are not implemented
        self.reg
            .f
            .set_wo_z(Flag::H_MASK | Flag::N_MASK | Flag::C_MASK, 0);
        self.reg.f.pv = PVFlag::Overflow(0);
    }

    fn affect_flag_outi_outd(&mut self, _opr: u8, b: u8, _l: u8) {
        self.reg.f.set_wo_z(Flag::F53_MASK | Flag::S_MASK, b);
        self.reg.f.zf = ZFlag::Acc(b);
        // TODO H, N, PV, C are not implemented
        self.reg
            .f
            .set_wo_z(Flag::H_MASK | Flag::N_MASK | Flag::C_MASK, 0);
        self.reg.f.pv = PVFlag::Overflow(0);
    }

    fn affect_flag_bit(&mut self, res: u8) {
        self.reg.f.set_wo_z(Flag::S_MASK | Flag::F53_MASK, res);
        let z = res == 0;
        self.reg.f.set_w_z(
            Flag::Z_MASK | Flag::H_MASK | Flag::N_MASK,
            Flag::H_MASK | ((z as u8) << 6),
        );
        self.reg.f.pv = PVFlag::from_bool(z);
    }

    fn run_op(&mut self, op: u8, f: fn(&mut Self, u8) -> Step) -> Step {
        self.reg.add_pc(1);
        f(self, op)
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
            self.reg.pc = self.mem_ref16(self.reg.sp);
            self.reg.sp = self.reg.sp.wrapping_add(2);
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
            self.reg.sp = self.reg.sp.wrapping_sub(2);
            self.mem_store16(self.reg.sp, self.reg.pc);
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
        self.reg.sp = self.reg.sp.wrapping_sub(2);
        self.mem_store16(self.reg.sp, self.reg.pc);
        self.reg.pc = (op & 0x38u8) as u16;
        Step::Run(11)
    }

    fn op_ret(&mut self, _: u8) -> Step {
        self.reg.pc = self.mem_ref16(self.reg.sp);
        self.reg.sp = self.reg.sp.wrapping_add(2);
        Step::Run(10)
    }

    fn op_call_nn(&mut self, _: u8) -> Step {
        let pc = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.reg.sp = self.reg.sp.wrapping_sub(2);
        self.mem_store16(self.reg.sp, self.reg.pc);
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

    fn op_sbc_a_n(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.pc);
        self.reg.add_pc(1);
        let res = (self.reg.a as u32)
            .wrapping_sub(opr as u32)
            .wrapping_sub(self.reg.f.c_bit() as u32);
        self.affect_flag_sub8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(7)
    }

    fn op_ex_ind_sp_hl(&mut self, _: u8) -> Step {
        let old_mem = self.mem_ref16(self.reg.sp);
        self.mem_store16(self.reg.sp, self.reg.hl);
        self.reg.hl = old_mem;
        Step::Run(19)
    }

    fn op_and_n(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.pc);
        self.reg.add_pc(1);
        let res = self.reg.a & opr;
        self.affect_flag_and(res);
        self.reg.a = res;
        Step::Run(7)
    }

    fn op_jp_hl(&mut self, _: u8) -> Step {
        self.reg.pc = self.reg.hl;
        Step::Run(4)
    }

    fn op_ex_de_hl(&mut self, _: u8) -> Step {
        std::mem::swap(&mut self.reg.de, &mut self.reg.hl);
        Step::Run(4)
    }

    fn op_xor_n(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.pc);
        self.reg.add_pc(1);
        let res = self.reg.a ^ opr;
        self.affect_flag_or_xor(res);
        self.reg.a = res;
        Step::Run(7)
    }

    fn op_di(&mut self, _: u8) -> Step {
        // TODO unimplemented
        Step::Run(4)
    }

    fn op_or_n(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.pc);
        self.reg.add_pc(1);
        let res = self.reg.a | opr;
        self.affect_flag_or_xor(res);
        self.reg.a = res;
        Step::Run(7)
    }

    fn op_ld_sp_hl(&mut self, _: u8) -> Step {
        self.reg.sp = self.reg.hl;
        Step::Run(6)
    }

    fn op_ei(&mut self, _: u8) -> Step {
        // TODO unimplemented
        Step::Run(4)
    }

    fn op_cp_n(&mut self, _: u8) -> Step {
        let opr = self.mem_ref8(self.reg.pc);
        self.reg.add_pc(1);
        let res = (self.reg.a as u32).wrapping_sub(opr as u32);
        self.affect_flag_cp(self.reg.a, opr, res);
        Step::Run(7)
    }

    pub fn step(&mut self) -> Step {
        let op = self.mem_ref8(self.reg.pc);
        let mut run_op = |f: fn(&mut Self, u8) -> Step| self.run_op(op, f);
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
            0xdd => run_op(Self::op_ix),
            0xde => run_op(Self::op_sbc_a_n),
            0xe3 => run_op(Self::op_ex_ind_sp_hl),
            0xe6 => run_op(Self::op_and_n),
            0xe9 => run_op(Self::op_jp_hl),
            0xeb => run_op(Self::op_ex_de_hl),
            0xed => run_op(Self::op_extended),
            0xee => run_op(Self::op_xor_n),
            0xf3 => run_op(Self::op_di),
            0xf6 => run_op(Self::op_or_n),
            0xf9 => run_op(Self::op_ld_sp_hl),
            0xfb => run_op(Self::op_ei),
            0xfd => run_op(Self::op_iy),
            0xfe => run_op(Self::op_cp_n),
            _ => Step::IllegalInstruction,
        }
    }

    fn op_in_f_ind_c(&mut self, _: u8) -> Step {
        // TODO unimplemented
        let res = 0x00u8;
        self.affect_flag_in(res);
        Step::Run(12)
    }

    fn op_in_r_ind_c(&mut self, op: u8) -> Step {
        // TODO unimplemented
        let res = 0x00u8;
        self.affect_flag_in(res);
        self.reg.set_reg8((op >> 3) & 7, res);
        Step::Run(12)
    }

    fn op_out_ind_c_zero(&mut self, _: u8) -> Step {
        // TODO unimplemented
        Step::Run(12)
    }

    fn op_out_ind_c_r(&mut self, _: u8) -> Step {
        // TODO unimplemented
        Step::Run(12)
    }

    fn op_sbc_hl_rr(&mut self, op: u8) -> Step {
        let opr = self.reg.reg16((op >> 4) & 0x03);
        let res = (self.reg.hl as u32)
            .wrapping_sub(opr as u32)
            .wrapping_sub(self.reg.f.c_bit() as u32);
        self.affect_flag_sub16(self.reg.hl, opr, res);
        self.reg.hl = res as u16;
        Step::Run(15)
    }

    fn op_ld_ind_nn_rr(&mut self, op: u8) -> Step {
        let nn = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.mem_store16(nn, self.reg.reg16((op >> 4) & 0x03));
        Step::Run(20)
    }

    fn op_neg(&mut self, _: u8) -> Step {
        let opr = self.reg.a;
        let res = 0u32.wrapping_sub(opr as u32);
        self.affect_flag_sub8(0u8, opr, res);
        self.reg.a = res as u8;
        Step::Run(8)
    }

    fn op_reti(&mut self, _: u8) -> Step {
        // TODO unimplemented
        self.reg.pc = self.mem_ref16(self.reg.sp);
        self.reg.sp = self.reg.sp.wrapping_add(2);
        Step::Run(14)
    }

    fn op_retn(&mut self, _: u8) -> Step {
        // TODO unimplemented
        self.reg.pc = self.mem_ref16(self.reg.sp);
        self.reg.sp = self.reg.sp.wrapping_add(2);
        Step::Run(14)
    }

    fn op_ld_i_a(&mut self, _: u8) -> Step {
        self.reg.i = self.reg.a;
        Step::Run(9)
    }

    fn op_adc_hl_rr(&mut self, op: u8) -> Step {
        let opr = self.reg.reg16((op >> 4) & 0x03);
        let res = self.reg.hl as u32 + opr as u32 + self.reg.f.c_bit() as u32;
        self.affect_flag_add16(self.reg.hl, opr, res);
        self.reg.hl = res as u16;
        Step::Run(15)
    }

    fn op_ld_rr_ind_nn(&mut self, op: u8) -> Step {
        let nn = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.reg.set_reg16((op >> 4) & 0x03, nn);
        Step::Run(20)
    }

    fn op_ld_r_a(&mut self, _: u8) -> Step {
        self.reg.r = self.reg.a;
        Step::Run(9)
    }

    fn op_ld_a_i(&mut self, _: u8) -> Step {
        self.reg.a = self.reg.i;
        self.affect_flag_ld_i_r(self.reg.a);
        Step::Run(9)
    }

    fn op_ld_a_r(&mut self, _: u8) -> Step {
        self.reg.a = self.reg.r;
        self.affect_flag_ld_i_r(self.reg.a);
        Step::Run(9)
    }

    fn op_rrd(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        self.mem_store8(self.reg.hl, (self.reg.a << 4) | (x >> 4));
        self.reg.a = (self.reg.a & 0xf0) | (x & 0x0f);
        self.affect_flag_rotate_decimal(self.reg.a);
        Step::Run(18)
    }

    fn op_rld(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        self.mem_store8(self.reg.hl, (self.reg.a & 0x0f) | (x << 4));
        self.reg.a = (self.reg.a & 0xf0) | (x >> 4);
        self.affect_flag_rotate_decimal(self.reg.a);
        Step::Run(18)
    }

    fn op_ldi(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        self.mem_store8(self.reg.de, x);
        self.reg.hl = self.reg.hl.wrapping_add(1);
        self.reg.de = self.reg.de.wrapping_add(1);
        self.reg.bc = self.reg.bc.wrapping_sub(1);
        self.affect_flag_ldi_ldd(self.reg.a, x, self.reg.bc);
        Step::Run(16)
    }

    fn op_cpi(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        self.reg.hl = self.reg.hl.wrapping_add(1);
        self.reg.bc = self.reg.bc.wrapping_sub(1);
        self.affect_flag_cpi_cpd(self.reg.a, x, self.reg.bc);
        Step::Run(16)
    }

    fn op_ini(&mut self, _: u8) -> Step {
        let x = 0x00u8; // TODO implement
        self.mem_store8(self.reg.hl, x);
        self.reg.hl = self.reg.hl.wrapping_add(1);
        self.reg.set_b(self.reg.b().wrapping_sub(1));
        self.affect_flag_ini_ind(x, self.reg.b(), self.reg.c());
        Step::Run(16)
    }

    fn op_outi(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        // TODO write _x to port C
        self.reg.hl = self.reg.hl.wrapping_add(1);
        self.reg.set_b(self.reg.b().wrapping_sub(1));
        self.affect_flag_outi_outd(x, self.reg.b(), self.reg.l());
        Step::Run(16)
    }

    fn op_ldd(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        self.mem_store8(self.reg.de, x);
        self.reg.hl = self.reg.hl.wrapping_sub(1);
        self.reg.de = self.reg.de.wrapping_sub(1);
        self.reg.bc = self.reg.bc.wrapping_sub(1);
        self.affect_flag_ldi_ldd(self.reg.a, x, self.reg.bc);
        Step::Run(16)
    }

    fn op_cpd(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        self.reg.hl = self.reg.hl.wrapping_sub(1);
        self.reg.bc = self.reg.bc.wrapping_sub(1);
        self.affect_flag_cpi_cpd(self.reg.a, x, self.reg.bc);
        Step::Run(16)
    }

    fn op_ind(&mut self, _: u8) -> Step {
        let x = 0x00u8; // TODO implement
        self.mem_store8(self.reg.hl, x);
        self.reg.hl = self.reg.hl.wrapping_sub(1);
        self.reg.set_b(self.reg.b().wrapping_sub(1));
        self.affect_flag_ini_ind(x, self.reg.b(), self.reg.c());
        Step::Run(16)
    }

    fn op_outd(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        // TODO write _x to port C
        self.reg.hl = self.reg.hl.wrapping_sub(1);
        self.reg.set_b(self.reg.b().wrapping_sub(1));
        self.affect_flag_outi_outd(x, self.reg.b(), self.reg.l());
        Step::Run(16)
    }

    fn op_repeat(&mut self, break_cond: bool, when_break: Step, when_repeat: Step) -> Step {
        if break_cond {
            when_break
        } else {
            self.reg.add_pc(0xfffe);
            when_repeat
        }
    }

    fn op_ldir(&mut self, op: u8) -> Step {
        self.op_ldi(op);
        self.op_repeat(self.reg.bc == 0, Step::Run(16), Step::Run(21))
    }

    fn op_cpir(&mut self, op: u8) -> Step {
        self.op_cpi(op);
        self.op_repeat(self.reg.bc == 0, Step::Run(16), Step::Run(21))
    }

    fn op_inir(&mut self, op: u8) -> Step {
        self.op_ini(op);
        self.op_repeat(self.reg.b() == 0, Step::Run(16), Step::Run(21))
    }

    fn op_otir(&mut self, op: u8) -> Step {
        self.op_outi(op);
        self.op_repeat(self.reg.b() == 0, Step::Run(16), Step::Run(21))
    }

    fn op_lddr(&mut self, op: u8) -> Step {
        self.op_ldd(op);
        self.op_repeat(self.reg.bc == 0, Step::Run(16), Step::Run(21))
    }

    fn op_cpdr(&mut self, op: u8) -> Step {
        self.op_cpd(op);
        self.op_repeat(self.reg.bc == 0, Step::Run(16), Step::Run(21))
    }

    fn op_indr(&mut self, op: u8) -> Step {
        self.op_ind(op);
        self.op_repeat(self.reg.b() == 0, Step::Run(16), Step::Run(21))
    }

    fn op_otdr(&mut self, op: u8) -> Step {
        self.op_outd(op);
        self.op_repeat(self.reg.b() == 0, Step::Run(16), Step::Run(21))
    }

    fn op_extended(&mut self, _: u8) -> Step {
        let op = self.mem_ref8(self.reg.pc);
        let mut run_op = |f: fn(&mut Self, u8) -> Step| self.run_op(op, f);
        match op {
            0x70 => run_op(Self::op_in_f_ind_c),
            op if op & 0xc7 == 0x40 => run_op(Self::op_in_r_ind_c),
            0x71 => run_op(Self::op_out_ind_c_zero),
            op if op & 0xc7 == 0x41 => run_op(Self::op_out_ind_c_r),
            op if op & 0xcf == 0x42 => run_op(Self::op_sbc_hl_rr),
            op if op & 0xcf == 0x43 => run_op(Self::op_ld_ind_nn_rr),
            op if op & 0xc7 == 0x44 => run_op(Self::op_neg),
            0x4d => run_op(Self::op_reti),
            op if op & 0xc7 == 0x45 => run_op(Self::op_retn),
            op if op & 0xc7 == 0x46 => Step::IllegalInstruction, // TODO im n
            0x47 => run_op(Self::op_ld_i_a),
            op if op & 0xcf == 0x4a => run_op(Self::op_adc_hl_rr),
            op if op & 0xcf == 0x4b => run_op(Self::op_ld_rr_ind_nn),
            0x4f => run_op(Self::op_ld_r_a),
            0x57 => run_op(Self::op_ld_a_i),
            0x5f => run_op(Self::op_ld_a_r),
            0x67 => run_op(Self::op_rrd),
            0x6f => run_op(Self::op_rld),
            0xa0 => run_op(Self::op_ldi),
            0xa1 => run_op(Self::op_cpi),
            0xa2 => run_op(Self::op_ini),
            0xa3 => run_op(Self::op_outi),
            0xa8 => run_op(Self::op_ldd),
            0xa9 => run_op(Self::op_cpd),
            0xaa => run_op(Self::op_ind),
            0xab => run_op(Self::op_outd),
            0xb0 => run_op(Self::op_ldir),
            0xb1 => run_op(Self::op_cpir),
            0xb2 => run_op(Self::op_inir),
            0xb3 => run_op(Self::op_otir),
            0xb8 => run_op(Self::op_lddr),
            0xb9 => run_op(Self::op_cpdr),
            0xba => run_op(Self::op_indr),
            0xbb => run_op(Self::op_otdr),
            _ => Step::IllegalInstruction,
        }
    }

    fn op_rlc_ind_hl(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        let res = (x << 1) | (x >> 7);
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(self.reg.hl, res);
        Step::Run(15)
    }

    fn op_rlc_r(&mut self, op: u8) -> Step {
        let x = self.reg.reg8(op & 0x07u8);
        let res = (x << 1) | (x >> 7);
        self.affect_flag_rotate_shift(res, x >> 7);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(8)
    }

    fn op_rrc_ind_hl(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        let res = (x >> 1) | (x << 7);
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(self.reg.hl, res);
        Step::Run(15)
    }

    fn op_rrc_r(&mut self, op: u8) -> Step {
        let x = self.reg.reg8(op & 0x07u8);
        let res = (x >> 1) | (x << 7);
        self.affect_flag_rotate_shift(res, x);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(8)
    }

    fn op_rl_ind_hl(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        let res = (x << 1) | self.reg.f.c_bit();
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(self.reg.hl, res);
        Step::Run(15)
    }

    fn op_rl_r(&mut self, op: u8) -> Step {
        let x = self.reg.reg8(op & 0x07u8);
        let res = (x << 1) | self.reg.f.c_bit();
        self.affect_flag_rotate_shift(res, x >> 7);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(8)
    }

    fn op_rr_ind_hl(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        let res = (x >> 1) | (self.reg.f.c_bit() << 7);
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(self.reg.hl, res);
        Step::Run(15)
    }

    fn op_rr_r(&mut self, op: u8) -> Step {
        let x = self.reg.reg8(op & 0x07u8);
        let res = (x >> 1) | (self.reg.f.c_bit() << 7);
        self.affect_flag_rotate_shift(res, x);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(8)
    }

    fn op_sla_ind_hl(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        let res = x << 1;
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(self.reg.hl, res);
        Step::Run(15)
    }

    fn op_sla_r(&mut self, op: u8) -> Step {
        let x = self.reg.reg8(op & 0x07u8);
        let res = x << 1;
        self.affect_flag_rotate_shift(res, x >> 7);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(8)
    }

    fn op_sra_ind_hl(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        let res = (x as i8 >> 1) as u8;
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(self.reg.hl, res);
        Step::Run(15)
    }

    fn op_sra_r(&mut self, op: u8) -> Step {
        let x = self.reg.reg8(op & 0x07u8);
        let res = (x as i8 >> 1) as u8;
        self.affect_flag_rotate_shift(res, x);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(8)
    }

    fn op_sll_ind_hl(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        let res = (x << 1) | 0x01u8;
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(self.reg.hl, res);
        Step::Run(15)
    }

    fn op_sll_r(&mut self, op: u8) -> Step {
        let x = self.reg.reg8(op & 0x07u8);
        let res = (x << 1) | 0x01u8;
        self.affect_flag_rotate_shift(res, x >> 7);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(8)
    }

    fn op_srl_ind_hl(&mut self, _: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        let res = x >> 1;
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(self.reg.hl, res);
        Step::Run(15)
    }

    fn op_srl_r(&mut self, op: u8) -> Step {
        let x = self.reg.reg8(op & 0x07u8);
        let res = x >> 1;
        self.affect_flag_rotate_shift(res, x);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(8)
    }

    fn op_bit_ind_hl(&mut self, op: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        let res = x & (0x01u8 << ((op >> 3) & 0x07));
        self.affect_flag_bit(res);
        Step::Run(12)
    }

    fn op_bit_r(&mut self, op: u8) -> Step {
        let x = self.reg.reg8(op & 0x07u8);
        let res = x & (0x01u8 << ((op >> 3) & 0x07));
        self.affect_flag_bit(res);
        Step::Run(8)
    }

    fn op_res_ind_hl(&mut self, op: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        let res = x & !(0x01u8 << ((op >> 3) & 0x07));
        self.mem_store8(self.reg.hl, res);
        Step::Run(15)
    }

    fn op_res_r(&mut self, op: u8) -> Step {
        let x = self.reg.reg8(op & 0x07u8);
        let res = x & !(0x01u8 << ((op >> 3) & 0x07));
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(8)
    }

    fn op_set_ind_hl(&mut self, op: u8) -> Step {
        let x = self.mem_ref8(self.reg.hl);
        let res = x | (0x01u8 << ((op >> 3) & 0x07));
        self.mem_store8(self.reg.hl, res);
        Step::Run(15)
    }

    fn op_set_r(&mut self, op: u8) -> Step {
        let x = self.reg.reg8(op & 0x07u8);
        let res = x | (0x01u8 << ((op >> 3) & 0x07));
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(8)
    }

    fn op_bits(&mut self, _: u8) -> Step {
        let op = self.mem_ref8(self.reg.pc);
        let mut run_op = |f: fn(&mut Self, u8) -> Step| self.run_op(op, f);
        match op {
            0x06 => run_op(Self::op_rlc_ind_hl),
            op if op & 0xf8 == 0x00 => run_op(Self::op_rlc_r),
            0x0c => run_op(Self::op_rrc_ind_hl),
            op if op & 0xf8 == 0x08 => run_op(Self::op_rrc_r),
            0x16 => run_op(Self::op_rl_ind_hl),
            op if op & 0xf8 == 0x10 => run_op(Self::op_rl_r),
            0x1c => run_op(Self::op_rr_ind_hl),
            op if op & 0xf8 == 0x18 => run_op(Self::op_rr_r),
            0x26 => run_op(Self::op_sla_ind_hl),
            op if op & 0xf8 == 0x20 => run_op(Self::op_sla_r),
            0x2c => run_op(Self::op_sra_ind_hl),
            op if op & 0xf8 == 0x28 => run_op(Self::op_sra_r),
            0x36 => run_op(Self::op_sll_ind_hl),
            op if op & 0xf8 == 0x30 => run_op(Self::op_sll_r),
            0x3c => run_op(Self::op_srl_ind_hl),
            op if op & 0xf8 == 0x38 => run_op(Self::op_srl_r),
            op if op & 0xc7 == 0x46 => run_op(Self::op_bit_ind_hl),
            op if op & 0xc0 == 0x40 => run_op(Self::op_bit_r),
            op if op & 0xc7 == 0x86 => run_op(Self::op_res_ind_hl),
            op if op & 0xc0 == 0x80 => run_op(Self::op_res_r),
            op if op & 0xc7 == 0xc6 => run_op(Self::op_set_ind_hl),
            op if op & 0xc0 == 0xc0 => run_op(Self::op_set_r),
            _ => Step::IllegalInstruction,
        }
    }

    fn op_ix(&mut self, _: u8) -> Step {
        self.op_ixy(IXY::IX)
    }

    fn op_iy(&mut self, _: u8) -> Step {
        self.op_ixy(IXY::IY)
    }

    fn op_add_ixy_rr(&mut self, ixy: IXY, op: u8) -> Step {
        let opr2 = self.reg.reg_ixy16(ixy, (op >> 4) & 0x03);
        let opr1 = self.reg.reg_ixy16(ixy, 2);
        let res = opr1 as u32 + opr2 as u32;
        self.affect_flag_add16(opr1, opr2, res);
        self.reg.set_reg_ixy16(ixy, 2, res as u16);
        Step::Run(15)
    }

    fn op_ld_ixy_nn(&mut self, ixy: IXY, op: u8) -> Step {
        let nn = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.reg.set_reg_ixy16(ixy, (op >> 4) & 0x03, nn);
        Step::Run(14)
    }

    fn op_ld_ind_nn_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let nn = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.mem_store16(nn, self.reg.reg_ixy16(ixy, 2));
        Step::Run(20)
    }

    fn op_inc_ixy(&mut self, ixy: IXY, op: u8) -> Step {
        let index = (op >> 4) & 0x03;
        let opr = self.reg.reg_ixy16(ixy, index);
        self.reg.set_reg_ixy16(ixy, index, opr.wrapping_add(1));
        Step::Run(10)
    }

    fn op_inc_ixy8(&mut self, ixy: IXY, op: u8) -> Step {
        let index = (op >> 3) & 0x07;
        let opr = self.reg.reg_ixy8(ixy, index);
        let res = opr as u32 + 1;
        self.affect_flag_inc8(opr, res);
        self.reg.set_reg_ixy8(ixy, index, res as u8);
        Step::Run(8)
    }

    fn op_dec_ixy8(&mut self, ixy: IXY, op: u8) -> Step {
        let index = (op >> 3) & 0x07;
        let opr = self.reg.reg_ixy8(ixy, index);
        let res = (opr as u32).wrapping_sub(1);
        self.affect_flag_dec8(opr, res);
        self.reg.set_reg_ixy8(ixy, index, res as u8);
        Step::Run(8)
    }

    fn op_ld_ixy8_n(&mut self, ixy: IXY, op: u8) -> Step {
        let n = self.mem_ref8(self.reg.pc);
        self.reg.set_reg_ixy8(ixy, (op >> 3) & 0x07, n);
        self.reg.add_pc(1);
        Step::Run(11)
    }

    fn op_ld_ixy_ind_nn(&mut self, ixy: IXY, _: u8) -> Step {
        let nn = self.mem_ref16(self.reg.pc);
        self.reg.add_pc(2);
        self.reg.set_reg_ixy16(ixy, 2, self.mem_ref16(nn));
        Step::Run(16)
    }

    fn op_dec_ixy(&mut self, ixy: IXY, op: u8) -> Step {
        let index = (op >> 4) & 0x03;
        let opr = self.reg.reg_ixy16(ixy, index);
        self.reg.set_reg_ixy16(ixy, index, opr.wrapping_sub(1));
        Step::Run(10)
    }

    fn op_inc_ind_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let opr = self.mem_ref8(addr);
        let res = opr as u32 + 1;
        self.affect_flag_inc8(opr, res);
        self.mem_store8(addr, res as u8);
        Step::Run(23)
    }

    fn op_dec_ind_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let opr = self.mem_ref8(addr);
        let res = (opr as u32).wrapping_sub(1);
        self.affect_flag_dec8(opr, res);
        self.mem_store8(addr, res as u8);
        Step::Run(23)
    }

    fn op_ld_ind_ixy_n(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let n = self.mem_ref8(self.reg.pc);
        self.mem_store8(addr, n);
        self.reg.add_pc(1);
        Step::Run(19)
    }

    fn op_ld_r_ind_ixy(&mut self, ixy: IXY, op: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        self.reg.set_reg8((op >> 3) & 0x07, self.mem_ref8(addr));
        Step::Run(19)
    }

    fn op_ld_ind_ixy_r(&mut self, ixy: IXY, op: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        self.mem_store8(addr, self.reg.reg8(op & 0x07));
        Step::Run(19)
    }

    fn op_ld_rixy8_rixy8(&mut self, ixy: IXY, op: u8) -> Step {
        let src = (op >> 3) & 0x07;
        self.reg
            .set_reg_ixy8(ixy, op & 0x07, self.reg.reg_ixy8(ixy, src));
        Step::Run(8)
    }

    fn op_add_a_ixy8(&mut self, ixy: IXY, op: u8) -> Step {
        let opr = self.reg.reg_ixy8(ixy, op & 0x07);
        let res = self.reg.a as u32 + opr as u32;
        self.affect_flag_add8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(8)
    }

    fn op_add_a_ind_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let opr = self.mem_ref8(addr);
        let res = self.reg.a as u32 + opr as u32;
        self.affect_flag_add8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(19)
    }

    fn op_adc_a_ixy8(&mut self, ixy: IXY, op: u8) -> Step {
        let opr = self.reg.reg_ixy8(ixy, op & 0x07);
        let res = self.reg.a as u32 + opr as u32 + self.reg.f.c_bit() as u32;
        self.affect_flag_add8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(8)
    }

    fn op_adc_a_ind_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let opr = self.mem_ref8(addr);
        let res = self.reg.a as u32 + opr as u32 + self.reg.f.c_bit() as u32;
        self.affect_flag_add8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(19)
    }

    fn op_sub_ixy8(&mut self, ixy: IXY, op: u8) -> Step {
        let opr = self.reg.reg_ixy8(ixy, op & 0x07);
        let res = (self.reg.a as u32).wrapping_sub(opr as u32);
        self.affect_flag_sub8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(8)
    }

    fn op_sub_ind_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let opr = self.mem_ref8(addr);
        let res = (self.reg.a as u32).wrapping_sub(opr as u32);
        self.affect_flag_sub8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(19)
    }

    fn op_sbc_a_ixy8(&mut self, ixy: IXY, op: u8) -> Step {
        let opr = self.reg.reg_ixy8(ixy, op & 0x07);
        let res = (self.reg.a as u32)
            .wrapping_sub(opr as u32)
            .wrapping_sub(self.reg.f.c_bit() as u32);
        self.affect_flag_sub8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(8)
    }

    fn op_sbc_a_ind_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let opr = self.mem_ref8(addr);
        let res = (self.reg.a as u32)
            .wrapping_sub(opr as u32)
            .wrapping_sub(self.reg.f.c_bit() as u32);
        self.affect_flag_sub8(self.reg.a, opr, res);
        self.reg.a = res as u8;
        Step::Run(19)
    }

    fn op_and_ixy8(&mut self, ixy: IXY, op: u8) -> Step {
        let res = self.reg.a & self.reg.reg_ixy8(ixy, op & 0x07);
        self.affect_flag_and(res);
        self.reg.a = res;
        Step::Run(8)
    }

    fn op_and_ind_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let res = self.reg.a & self.mem_ref8(addr);
        self.affect_flag_and(res);
        self.reg.a = res;
        Step::Run(19)
    }

    fn op_xor_ixy8(&mut self, ixy: IXY, op: u8) -> Step {
        let res = self.reg.a ^ self.reg.reg_ixy8(ixy, op & 0x07);
        self.affect_flag_or_xor(res);
        self.reg.a = res;
        Step::Run(8)
    }

    fn op_xor_ind_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let res = self.reg.a ^ self.mem_ref8(addr);
        self.affect_flag_or_xor(res);
        self.reg.a = res;
        Step::Run(19)
    }

    fn op_or_ixy8(&mut self, ixy: IXY, op: u8) -> Step {
        let res = self.reg.a | self.reg.reg_ixy8(ixy, op & 0x07);
        self.affect_flag_or_xor(res);
        self.reg.a = res;
        Step::Run(8)
    }

    fn op_or_ind_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let res = self.reg.a | self.mem_ref8(addr);
        self.affect_flag_or_xor(res);
        self.reg.a = res;
        Step::Run(19)
    }

    fn op_cp_ixy8(&mut self, ixy: IXY, op: u8) -> Step {
        let opr = self.reg.reg_ixy8(ixy, op & 0x07);
        let res = (self.reg.a as u32).wrapping_sub(opr as u32);
        self.affect_flag_cp(self.reg.a, opr, res);
        Step::Run(8)
    }

    fn op_cp_ind_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let opr = self.mem_ref8(addr);
        let res = (self.reg.a as u32).wrapping_sub(opr as u32);
        self.affect_flag_cp(self.reg.a, opr, res);
        Step::Run(19)
    }

    fn op_ixy(&mut self, ixy: IXY) -> Step {
        let op = self.mem_ref8(self.reg.pc);
        let mut run_op = |f: fn(&mut Self, IXY, u8) -> Step| {
            self.reg.add_pc(1);
            f(self, ixy, op)
        };
        match op {
            op if op & 0xcf == 0x09 => run_op(Self::op_add_ixy_rr),
            0x21 => run_op(Self::op_ld_ixy_nn),
            0x22 => run_op(Self::op_ld_ind_nn_ixy),
            0x23 => run_op(Self::op_inc_ixy),
            op if op & 0xf7 == 0x24 => run_op(Self::op_inc_ixy8),
            op if op & 0xf7 == 0x25 => run_op(Self::op_dec_ixy8),
            op if op & 0xf7 == 0x26 => run_op(Self::op_ld_ixy8_n),
            0x2a => run_op(Self::op_ld_ixy_ind_nn),
            0x2b => run_op(Self::op_dec_ixy),
            0x34 => run_op(Self::op_inc_ind_ixy),
            0x35 => run_op(Self::op_dec_ind_ixy),
            0x36 => run_op(Self::op_ld_ind_ixy_n),
            0x76 => Step::IllegalInstruction,
            op if op & 0xc7 == 0x46 => run_op(Self::op_ld_r_ind_ixy),
            op if op & 0xf8 == 0x70 => run_op(Self::op_ld_ind_ixy_r),
            op if op & 0xf0 == 0x60 => run_op(Self::op_ld_rixy8_rixy8),
            op if op & 0xe6 == 0x44 => run_op(Self::op_ld_rixy8_rixy8),
            op if op & 0xfe == 0x84 => run_op(Self::op_add_a_ixy8),
            0x86 => run_op(Self::op_add_a_ind_ixy),
            op if op & 0xfe == 0x8c => run_op(Self::op_adc_a_ixy8),
            0x8e => run_op(Self::op_adc_a_ind_ixy),
            op if op & 0xfe == 0x94 => run_op(Self::op_sub_ixy8),
            0x96 => run_op(Self::op_sub_ind_ixy),
            op if op & 0xfe == 0x9c => run_op(Self::op_sbc_a_ixy8),
            0x9e => run_op(Self::op_sbc_a_ind_ixy),
            op if op & 0xfe == 0xa4 => run_op(Self::op_and_ixy8),
            0xa6 => run_op(Self::op_and_ind_ixy),
            op if op & 0xfe == 0xac => run_op(Self::op_xor_ixy8),
            0xae => run_op(Self::op_xor_ind_ixy),
            op if op & 0xfe == 0xb4 => run_op(Self::op_or_ixy8),
            0xb6 => run_op(Self::op_or_ind_ixy),
            op if op & 0xfe == 0xbc => run_op(Self::op_cp_ixy8),
            0xbe => run_op(Self::op_cp_ind_ixy),
            0xcb => run_op(Self::op_bits_ixy),
            _ => Step::IllegalInstruction,
        }
    }

    fn op_rlc_ind_ixy(&mut self, addr: u16, _: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x << 1) | (x >> 7);
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(addr, res);
        Step::Run(23)
    }

    fn op_rlc_ind_ixy_r(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x << 1) | (x >> 7);
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(addr, res);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(23)
    }

    fn op_rrc_ind_ixy(&mut self, addr: u16, _: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x >> 1) | (x << 7);
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(addr, res);
        Step::Run(23)
    }

    fn op_rrc_ind_ixy_r(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x >> 1) | (x << 7);
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(addr, res);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(23)
    }

    fn op_rl_ind_ixy(&mut self, addr: u16, _: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x << 1) | self.reg.f.c_bit();
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(addr, res);
        Step::Run(15)
    }

    fn op_rl_ind_ixy_r(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x << 1) | self.reg.f.c_bit();
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(addr, res);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(23)
    }

    fn op_rr_ind_ixy(&mut self, addr: u16, _: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x >> 1) | (self.reg.f.c_bit() << 7);
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(addr, res);
        Step::Run(23)
    }

    fn op_rr_ind_ixy_r(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x >> 1) | (self.reg.f.c_bit() << 7);
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(addr, res);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(23)
    }

    fn op_sla_ind_ixy(&mut self, addr: u16, _: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = x << 1;
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(addr, res);
        Step::Run(23)
    }

    fn op_sla_ind_ixy_r(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = x << 1;
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(addr, res);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(23)
    }

    fn op_sra_ind_ixy(&mut self, addr: u16, _: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x as i8 >> 1) as u8;
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(addr, res);
        Step::Run(23)
    }

    fn op_sra_ind_ixy_r(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x as i8 >> 1) as u8;
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(addr, res);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(23)
    }

    fn op_sll_ind_ixy(&mut self, addr: u16, _: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x << 1) | 0x01u8;
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(addr, res);
        Step::Run(23)
    }

    fn op_sll_ind_ixy_r(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = (x << 1) | 0x01u8;
        self.affect_flag_rotate_shift(res, x >> 7);
        self.mem_store8(addr, res);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(23)
    }

    fn op_srl_ind_ixy(&mut self, addr: u16, _: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = x >> 1;
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(addr, res);
        Step::Run(23)
    }

    fn op_srl_ind_ixy_r(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = x >> 1;
        self.affect_flag_rotate_shift(res, x);
        self.mem_store8(addr, res);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(23)
    }

    fn op_bit_ind_ixy(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = x & (0x01u8 << ((op >> 3) & 0x07));
        self.affect_flag_bit(res);
        Step::Run(20)
    }

    fn op_res_ind_ixy(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = x & !(0x01u8 << ((op >> 3) & 0x07));
        self.mem_store8(addr, res);
        Step::Run(23)
    }

    fn op_res_ind_ixy_r(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = x & !(0x01u8 << ((op >> 3) & 0x07));
        self.mem_store8(addr, res);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(23)
    }

    fn op_set_ind_ixy(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = x | (0x01u8 << ((op >> 3) & 0x07));
        self.mem_store8(addr, res);
        Step::Run(23)
    }

    fn op_set_ind_ixy_r(&mut self, addr: u16, op: u8) -> Step {
        let x = self.mem_ref8(addr);
        let res = x | (0x01u8 << ((op >> 3) & 0x07));
        self.mem_store8(addr, res);
        self.reg.set_reg8(op & 0x07u8, res);
        Step::Run(23)
    }

    fn op_bits_ixy(&mut self, ixy: IXY, _: u8) -> Step {
        let addr = self.ixy_offset(ixy);
        let op = self.mem_ref8(self.reg.pc);
        let mut run_op = |f: fn(&mut Self, u16, u8) -> Step| {
            self.reg.add_pc(1);
            f(self, addr, op)
        };
        match op {
            0x06 => run_op(Self::op_rlc_ind_ixy),
            op if op & 0xf8 == 0x00 => run_op(Self::op_rlc_ind_ixy_r),
            0x0c => run_op(Self::op_rrc_ind_ixy),
            op if op & 0xf8 == 0x08 => run_op(Self::op_rrc_ind_ixy_r),
            0x16 => run_op(Self::op_rl_ind_ixy),
            op if op & 0xf8 == 0x10 => run_op(Self::op_rl_ind_ixy_r),
            0x1c => run_op(Self::op_rr_ind_ixy),
            op if op & 0xf8 == 0x18 => run_op(Self::op_rr_ind_ixy_r),
            0x26 => run_op(Self::op_sla_ind_ixy),
            op if op & 0xf8 == 0x20 => run_op(Self::op_sla_ind_ixy_r),
            0x2c => run_op(Self::op_sra_ind_ixy),
            op if op & 0xf8 == 0x28 => run_op(Self::op_sra_ind_ixy_r),
            0x36 => run_op(Self::op_sll_ind_ixy),
            op if op & 0xf8 == 0x30 => run_op(Self::op_sll_ind_ixy_r),
            0x3c => run_op(Self::op_srl_ind_ixy),
            op if op & 0xf8 == 0x38 => run_op(Self::op_srl_ind_ixy_r),
            op if op & 0xc0 == 0x40 => run_op(Self::op_bit_ind_ixy),
            op if op & 0xc7 == 0x86 => run_op(Self::op_res_ind_ixy),
            op if op & 0xc0 == 0x80 => run_op(Self::op_res_ind_ixy_r),
            op if op & 0xc7 == 0xc6 => run_op(Self::op_set_ind_ixy),
            op if op & 0xc0 == 0xc0 => run_op(Self::op_set_ind_ixy_r),
            _ => Step::IllegalInstruction,
        }
    }
}
