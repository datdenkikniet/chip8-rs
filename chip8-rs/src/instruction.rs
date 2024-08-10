use chip8_rs_macros::instructions;

use crate::register::Register;
use crate::{Address, Nibble};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Sys(Address),
    Cls,
    Ret,
    Jp(Address),
    Call(Address),
    SeVal(Register, u8),
    SneVal(Register, u8),
    SeReg(Register, Register),
    LdVal(Register, u8),
    AddVal(Register, u8),
    LdReg(Register, Register),
    Or(Register, Register),
    And(Register, Register),
    Xor(Register, Register),
    AddReg(Register, Register),
    Sub(Register, Register),
    Shr(Register, Register),
    Subn(Register, Register),
    Shl(Register, Register),
    SneReg(Register, Register),
    LdI(Address),
    JpOffset(Address),
    Rnd(Register, u8),
    Drw(Register, Register, Nibble),
    Skp(Register),
    Sknp(Register),
    LdDtToReg(Register),
    LdKey(Register),
    LdDt(Register),
    LdSt(Register),
    AddI(Register),
    LdF(Register),
    LdB(Register),
    LdContToMem(Register),
    LdContFromMem(Register),

    // Extended
    Exit,
}

#[allow(dead_code)]
const fn assert_u8_nibble(value: u8) {
    if Nibble::new(value).is_none() {
        panic!("Value must be a nibble");
    }
}

macro_rules! decode {
    () => {};

    ($ident:ident, $instr:expr, $x:ident, $y:ident, $k:ident, $n:ident, $a:ident, value($value:expr)) => {
        if $instr == $value {
            return Some(Instruction::$ident);
        }
    };

    ($ident:ident, $instr:expr, $x:ident, $y:ident, $k:ident, $n:ident, $a:ident, addr($v:literal, a)) => {
        if (($instr >> 12) & 0xF) == $v {
            return Some(Instruction::$ident($a));
        }
    };

    ($ident:ident, $instr:expr, $x:ident, $y:ident, $k:ident, $n:ident, $a:ident, xk($v:literal, x, k)) => {
        if (($instr >> 12) & 0xF) == $v {
            return Some(Instruction::$ident($x, $k));
        }
    };

    ($ident:ident, $instr:expr, $x:ident, $y:ident, $k:ident, $n:ident, $a:ident, xy($v1:literal, x, y, $v2:literal)) => {
        if (($instr >> 12) & 0xF) == ($v1 << 12) && ($instr & 0xF) == $v2 {
            return Some(Instruction::$ident($x, $y));
        }
    };

    ($ident:ident, $instr:expr, $x:ident, $y:ident, $k:ident, $n:ident, $a:ident, x($v1:literal, x, $v2:literal)) => {
        if (($instr >> 12) & 0xF) == $v1 && ($instr & 0xFF) == $v2 {
            return Some(Instruction::$ident($x));
        }
    };

    ($ident:ident, $instr:expr, $x:ident, $y:ident, $k:ident, $n:ident, $a:ident, xyn($v1:literal, x, y, n)) => {
        if (($instr >> 12) & 0xF) == $v1 {
            return Some(Instruction::$ident($x, $y, $n));
        }
    };

    ($instr:ident, $(Instruction::$ident:ident$(($($field:ident),*))? => $encoding_ty:ident($($encoding:tt)*),)*) => {
        let x = Nibble::new_truncate((($instr & 0x0F00) >> 8) as u8);
        let y = Nibble::new_truncate((($instr & 0x00F0) >> 4) as u8);

        let x = Register::from(x);
        let y = Register::from(y);
        let k = ($instr & 0xFF) as u8;
        let n = Nibble::new_truncate($instr as u8);
        let a = Address::new_truncate(0x00);

        $(
            decode!($ident, $instr, x, y, k, n, a, $encoding_ty($($encoding)*));
        )*
    }
}

macro_rules! encode {
    (value($v:literal)) => {
        $v
    };

    (addr($v:literal, $a:ident)) => {{
        const _: () = assert_u8_nibble($v);
        (($v as u16) << 12) | Address::get($a)
    }};

    (xk($v:literal, $x:ident, $k:ident)) => {{
        const _: () = assert_u8_nibble($v);
        let k: u16 = u8::into(*$k);
        (($v as u16) << 12) | ((*$x as u16) << 8) | k
    }};

    (xy($v0:literal, $x:ident, $y:ident, $v1:literal)) => {{
        const _: () = assert_u8_nibble($v0);
        const _: () = assert_u8_nibble($v1);
        $v0 | (Register::value($x) << 8) | (Register::value($y) << 4) | $v1 as u16
    }};

    (xyn($v0:literal, $x:ident, $y:ident, $v1:expr)) => {{
        const _: () = assert_u8_nibble($v0);
        $v0 | (Register::value($x) << 8) | (Register::value($y) << 4) | Nibble::get($v1) as u16
    }};

    (x($v0:literal, $x:ident, $v1:expr)) => {{
        const _: () = assert_u8_nibble($v0);
        let v1: u16 = u8::into($v1);
        ($v0 << 12) | (Register::value($x) << 8) | v1
    }};

    ($value:ident, $($pat:pat => $encoding_ty:ident($($encoding:tt)*),)*) => {
        match $value {
            $(
                $pat => encode!($encoding_ty($($encoding)*)),
            )*
        }
    }
}

macro_rules! gen_instructions {
    ($($tail:tt)*) => {
        impl Instruction {
            pub fn encode(&self) -> u16 {
                encode!(self, $($tail)*)
            }

            pub fn decode(value: u16) -> Option<Self> {
                decode!(value, $($tail)*);
                None
            }
        }
    };
}

instructions! {
    Cls => value(0x00E0),
    Ret => value(0x00EE)
}

gen_instructions! {
    Instruction::Cls => value(0x00E0),
    Instruction::Ret => value(0x00EE),
    Instruction::Exit => value(0x00FD),
    Instruction::Sys(a) => addr(0x0, a),
    Instruction::Jp(a) => addr(0x1, a),
    Instruction::Call(a) => addr(0x2, a),
    Instruction::SeVal(x, k) => xk(0x3, x, k),
    Instruction::SneVal(x, k) => xk(0x4, x, k),
    Instruction::SeReg(x, y) => xy(0x5, x, y, 0x0),
    Instruction::LdVal(x, k) => xk(0x6, x, k),
    Instruction::AddVal(x, k) => xk(0x7, x, k),
    Instruction::LdReg(x, y) => xy(0x8, x, y, 0x0),
    Instruction::Or(x, y) => xy(0x8, x, y, 0x1),
    Instruction::And(x, y) => xy(0x8, x, y, 0x2),
    Instruction::Xor(x, y) => xy(0x8, x, y, 0x3),
    Instruction::AddReg(x, y) => xy(0x8, x, y, 0x4),
    Instruction::Sub(x, y) => xy(0x8, x, y, 0x5),
    Instruction::Shr(x, y) => xy(0x8, x, y, 0x6),
    Instruction::Subn(x, y) => xy(0x8, x, y, 0x7),
    Instruction::Shl(x, y) => xy(0x8, x, y, 0xE),
    Instruction::SneReg(x, y) => xy(0x9, x, y, 0x0),
    Instruction::LdI(a) => addr(0xA, a),
    Instruction::JpOffset(a) => addr(0xB, a),
    Instruction::Rnd(x, k) => xk(0xC, x, k),
    Instruction::Drw(x, y, n) => xyn(0xD, x, y, n),
    Instruction::Skp(x) => x(0xE, x, 0x9E),
    Instruction::Sknp(x) => x(0xE, x, 0xA1),
    Instruction::LdDtToReg(x) => x(0xF, x, 0x07),
    Instruction::LdKey(x) => x(0xF, x, 0x0A),
    Instruction::LdDt(x) => x(0xF, x, 0x15),
    Instruction::LdSt(x) => x(0xF, x, 0x18),
    Instruction::AddI(x) => x(0xF, x, 0x1E),
    Instruction::LdF(x) => x(0xF, x, 0x29),
    Instruction::LdB(x) => x(0xF, x, 0x33),
    Instruction::LdContToMem(x) => x(0xF, x, 0x55),
    Instruction::LdContFromMem(x) => x(0xF, x, 0x65),
}
