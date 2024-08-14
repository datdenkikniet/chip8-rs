use crate::register::Register;
use crate::{Address, Nibble};

#[allow(dead_code)]
const fn assert_u8_nibble(value: u8) {
    if Nibble::new(value).is_none() {
        panic!("Value must be a nibble");
    }
}

macro_rules! decode {
    () => {};

    (x($instr:expr)) => {{ Register::from(Nibble::new_truncate((($instr & 0x0F00) >> 8) as u8)) }};
    (y($instr:expr)) => {{ Register::from(Nibble::new_truncate((($instr & 0x00F0) >> 4) as u8)) }};
    (n($instr:expr)) => {{ Nibble::new_truncate($instr as u8) }};
    (k($instr:expr)) => {{ ($instr & 0xFF) as u8 }};

    ($ident:ident, $instr:expr, value($value:expr)) => {
        if $instr == $value {
            return Some(Instruction::$ident);
        }
    };

    ($ident:ident, $instr:expr, addr($v:literal, $a:ident)) => {
        if (($instr >> 12) & 0xF) == $v {
            return Some(Instruction::$ident { $a: Address::new_truncate($instr) });
        }
    };

    ($ident:ident, $instr:expr, xk($v:literal, $x:ident, $k:ident)) => {
        if (($instr >> 12) & 0xF) == $v {
            return Some(Instruction::$ident { $x: decode!(x($instr)), $k: decode!(k($instr)) });
        }
    };

    ($ident:ident, $instr:expr, xy($v1:literal, $x:ident, $y:ident, $v2:literal)) => {
        if (($instr >> 12) & 0xF) == ($v1 << 12) && ($instr & 0xF) == $v2 {
            return Some(Instruction::$ident { $x: decode!(x($instr)), $y: decode!(y($instr)) });
        }
    };

    ($ident:ident, $instr:expr, x($v1:literal, $x:ident, $v2:literal)) => {
        if (($instr >> 12) & 0xF) == $v1 && ($instr & 0xFF) == $v2 {
            return Some(Instruction::$ident { $x: decode!(x($instr)) });
        }
    };

    ($ident:ident, $instr:expr, xyn($v1:literal, $x:ident, $y:ident, $n:ident)) => {
        if (($instr >> 12) & 0xF) == $v1 {
            return Some(Instruction::$ident{ $x: decode!(x($instr)), $y: decode!(y($instr)), $n: decode!(n($instr)) });
        }
    };

    ($instr:ident, $(Instruction::$ident:ident $( { $($field:ident),* } )? => $encoding_ty:ident($($encoding:tt),*)),*) => {
        $(
            decode!($ident, $instr, $encoding_ty($($encoding),*));
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
        (($v0 as u16) << 12) | (Register::value($x) << 8) | (Register::value($y) << 4) | $v1 as u16
    }};

    (xyn($v0:literal, $x:ident, $y:ident, $v1:expr)) => {{
        const _: () = assert_u8_nibble($v0);
        (($v0 as u16) << 12) | (Register::value($x) << 8) | (Register::value($y) << 4) | Nibble::get($v1) as u16
    }};

    (x($v0:literal, $x:ident, $v1:expr)) => {{
        const _: () = assert_u8_nibble($v0);
        let v1: u16 = u8::into($v1);
        ($v0 << 12) | (Register::value($x) << 8) | v1
    }};

    ($value:ident, $($pat:pat => $type:ident($($definition:tt),*)),*) => {
        match $value {
            $(
                $pat => encode!($type($($definition),*)),
            )*
        }
    }
}

macro_rules! gen_instructions2 {
    ($($name:ident $({$($field:ident: $ty:ty),*})? => $type:ident($($definition:tt),*)),*$(,)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum Instruction {
            $(
                $name$({$($field: $ty,)*})?,
            )*
        }

        impl Instruction {
            pub fn encode(&self) -> u16 {
                encode!(self, $(Instruction::$name $( { $($field,)* } )? => $type($($definition),*)),*)
            }

            pub fn decode(value: u16) -> Option<Self> {
                decode!(value, $(Instruction::$name $( { $($field),* } )* => $type($($definition),*)),*);
                None
            }
        }
    };
}

gen_instructions2! {
    Cls => value(0x00E0),
    Ret => value(0x00EE),
    Exit => value(0x00FD),
    Sys { address: Address } => addr(0x0, address),
    Jp { address: Address }=> addr(0x1, address),
    Call { address: Address } => addr(0x2, address),
    SeVal { x: Register, k: u8 } => xk(0x3, x, k),
    SneVal { x: Register, k: u8 } => xk(0x4, x, k),
    SeReg { x: Register, y: Register } => xy(0x5, x, y, 0x0),
    LdVal { x: Register, k: u8 } => xk(0x6, x, k),
    AddVal { x: Register, k: u8 } => xk(0x7, x, k),
    LdReg { x: Register, y: Register } => xy(0x8, x, y, 0x0),
    Or { x: Register, y: Register } => xy(0x8, x, y, 0x1),
    And { x: Register, y: Register } => xy(0x8, x, y, 0x2),
    Xor { x: Register, y: Register } => xy(0x8, x, y, 0x3),
    AddReg { x: Register, y: Register } => xy(0x8, x, y, 0x4),
    Sub { x: Register, y: Register } => xy(0x8, x, y, 0x5),
    Shr { x: Register, y: Register } => xy(0x8, x, y, 0x6),
    Subn { x: Register, y: Register } => xy(0x8, x, y, 0x7),
    Shl { x: Register, y: Register } => xy(0x8, x, y, 0xE),
    SneReg { x: Register, y: Register } => xy(0x9, x, y, 0x0),
    LdI { address: Address } => addr(0xA, address),
    JpOffset { offset: Address } => addr(0xB, offset),
    Rnd { x: Register, k: u8 } => xk(0xC, x, k),
    Drw { x: Register, y: Register, n: Nibble } => xyn(0xD, x, y, n),
    Skp { x: Register } => x(0xE, x, 0x9E),
    Sknp { x: Register } => x(0xE, x, 0xA1),
    LdDtToReg { x: Register } => x(0xF, x, 0x07),
    LdKey { x: Register } => x(0xF, x, 0x0A),
    LdDt { x: Register } => x(0xF, x, 0x15),
    LdSt { x: Register } => x(0xF, x, 0x18),
    AddI { x: Register } => x(0xF, x, 0x1E),
    LdF { x: Register } => x(0xF, x, 0x29),
    LdB { x: Register } => x(0xF, x, 0x33),
    LdContToMem { x: Register } => x(0xF, x, 0x55),
    LdContFromMem { x: Register } => x(0xF, x, 0x65),
}

#[test]
fn round_trip() {
    for i in 0..u16::MAX {
        if let Some(instr) = Instruction::decode(i) {
            let encoded = instr.encode();
            assert_eq!(encoded, i, "{:?}", instr);
        }
    }
}
