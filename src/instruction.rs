use crate::register::Register;

/// A 12-bit address value.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Address(u16);

impl Address {
    pub const fn new(value: u16) -> Option<Self> {
        if value > 0xFFF {
            None
        } else {
            Some(Self(value))
        }
    }

    pub const fn new_truncate(value: u16) -> Self {
        Self(value & 0xFFF)
    }

    pub const fn get(&self) -> u16 {
        self.0
    }

    pub const fn address(&self) -> usize {
        self.0 as usize
    }

    pub fn add(&mut self, value: u8) {
        self.0 = (self.0 + value as u16) % 0xFFF
    }

    pub fn inc(&mut self) {
        self.add(1);
    }
}

impl std::fmt::UpperHex for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04X}", self.0)
    }
}

impl std::fmt::LowerHex for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04x}", self.0)
    }
}

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::UpperHex::fmt(&self, f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Nibble(u8);

impl Nibble {
    pub const fn new(value: u8) -> Option<Self> {
        if value > 0xF {
            None
        } else {
            Some(Self(value))
        }
    }

    pub const fn new_truncate(value: u8) -> Self {
        Self(value & 0xF)
    }

    pub const fn get(&self) -> u8 {
        self.0
    }
}

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

impl Instruction {
    pub fn encode(&self) -> u16 {
        let mut x_out = None;
        let mut y_out = None;
        let mut k_out = None;
        let mut addr_out = None;

        macro_rules! encode {
            (addr($a:ident), $v:expr) => {{
                addr_out = Some($a);
                $v
            }};

            (xk($x:ident, $k:ident), $v:expr) => {{
                x_out = Some($x);
                k_out = Some($k);
                $v
            }};

            (xy($x:ident, $y:ident), $v:expr) => {{
                x_out = Some($x);
                y_out = Some($y);
                $v
            }};

            (x($x:ident), $v:expr) => {{
                x_out = Some($x);
                $v
            }};
        }

        let mut base: u16 = match self {
            Instruction::Sys(a) => encode!(addr(a), 0x0000),
            Instruction::Cls => 0x00E0,
            Instruction::Ret => 0x00EE,
            Instruction::Jp(a) => encode!(addr(a), 0x1000),
            Instruction::Call(a) => encode!(addr(a), 0x2000),
            Instruction::SeVal(x, k) => encode!(xk(x, k), 0x3000),
            Instruction::SneVal(x, k) => encode!(xk(x, k), 0x4000),
            Instruction::SeReg(x, y) => encode!(xy(x, y), 0x5000),
            Instruction::LdVal(x, k) => encode!(xk(x, k), 0x6000),
            Instruction::AddVal(x, k) => encode!(xk(x, k), 0x7000),
            Instruction::LdReg(x, y) => encode!(xy(x, y), 0x8000),
            Instruction::Or(x, y) => encode!(xy(x, y), 0x8001),
            Instruction::And(y, x) => encode!(xy(x, y), 0x8002),
            Instruction::Xor(x, y) => encode!(xy(x, y), 0x8003),
            Instruction::AddReg(x, y) => encode!(xy(x, y), 0x8004),
            Instruction::Sub(x, y) => encode!(xy(x, y), 0x8005),
            Instruction::Shr(x, y) => encode!(xy(x, y), 0x8006),
            Instruction::Subn(x, y) => encode!(xy(x, y), 0x8007),
            Instruction::Shl(x, y) => encode!(xy(x, y), 0x800E),
            Instruction::SneReg(x, y) => encode!(xy(x, y), 0x9000),
            Instruction::LdI(a) => encode!(addr(a), 0xA000),
            Instruction::JpOffset(a) => encode!(addr(a), 0xB000),
            Instruction::Rnd(x, k) => encode!(xk(x, k), 0xC000),
            Instruction::Drw(x, y, n) => {
                x_out = Some(x);
                y_out = Some(y);
                0xD000 | n.get() as u16
            }
            Instruction::Skp(x) => encode!(x(x), 0xE09E),
            Instruction::Sknp(x) => encode!(x(x), 0xE0A1),
            Instruction::LdDtToReg(x) => encode!(x(x), 0xF007),
            Instruction::LdKey(x) => encode!(x(x), 0xF00A),
            Instruction::LdDt(x) => encode!(x(x), 0xF00A),
            Instruction::LdSt(x) => encode!(x(x), 0xF018),
            Instruction::AddI(x) => encode!(x(x), 0xF01E),
            Instruction::LdF(x) => encode!(x(x), 0xF029),
            Instruction::LdB(x) => encode!(x(x), 0xF033),
            Instruction::LdContToMem(x) => encode!(x(x), 0xF055),
            Instruction::LdContFromMem(x) => encode!(x(x), 0xF065),
            Instruction::Exit => 0x00FD,
        };

        if let Some(x) = x_out {
            debug_assert!(addr_out.is_none());
            base |= (*x as u16) << 8;
        }

        if let Some(y) = y_out {
            debug_assert!(k_out.is_none());
            debug_assert!(addr_out.is_none());

            base |= (*y as u16) << 4;
        }

        if let Some(addr) = addr_out {
            debug_assert!(y_out.is_none());
            debug_assert!(x_out.is_none());
            debug_assert!(k_out.is_none());
            base |= addr.get() & 0xFFF;
        }

        if let Some(k_out) = k_out {
            debug_assert!(y_out.is_none());
            base |= *k_out as u16;
        }

        debug_assert_eq!(self, &Self::decode(base).unwrap());

        base
    }

    pub fn decode(instr_val: u16) -> Option<Self> {
        let addr = Address::new(instr_val & 0xFFF).unwrap();

        let x = Nibble::new(((instr_val & 0x0F00) >> 8) as u8).unwrap();
        let y = Nibble::new(((instr_val & 0x00F0) >> 4) as u8).unwrap();

        let x = Register::from(x);
        let y = Register::from(y);
        let k = (instr_val & 0xFF) as u8;

        let first_nibble = (instr_val & 0xF000) >> 12;
        let last = instr_val & 0x000F;
        let last_nibble = Nibble::new(last as u8).unwrap();
        let last_byte = (instr_val & 0xFF) as u8;

        let instr = if instr_val == 0x00E0 {
            Self::Cls
        } else if instr_val == 0x00EE {
            Self::Ret
        } else if instr_val == 0x00FD {
            Self::Exit
        } else {
            match first_nibble {
                0x0 => Self::Sys(addr),
                0x1 => Self::Jp(addr),
                0x2 => Self::Call(addr),
                0x3 => Self::SeVal(x, k),
                0x4 => Self::SneVal(x, k),
                0x5 if last == 0 => Self::SeReg(x, y),
                0x6 => Self::LdVal(x, k),
                0x7 => Self::AddVal(x, k),
                0x8 if last == 0 => Self::LdReg(x, y),
                0x8 if last == 1 => Self::Or(x, y),
                0x8 if last == 2 => Self::And(x, y),
                0x8 if last == 3 => Self::Xor(x, y),
                0x8 if last == 4 => Self::AddReg(x, y),
                0x8 if last == 5 => Self::Sub(x, y),
                0x8 if last == 6 => Self::Shr(x, y),
                0x8 if last == 7 => Self::Subn(x, y),
                0x8 if last == 0xE => Self::Shl(x, y),
                0x9 if last == 0 => Self::SneReg(x, y),
                0xA => Self::LdI(addr),
                0xB => Self::JpOffset(addr),
                0xC => Self::Rnd(x, k),
                0xD => Self::Drw(x, y, last_nibble),
                0xE if last_byte == 0x9E => Self::Skp(x),
                0xE if last_byte == 0xA1 => Self::Sknp(x),
                0xF if last_byte == 0x07 => Self::LdDtToReg(x),
                0xF if last_byte == 0x0A => Self::LdKey(x),
                0xF if last_byte == 0x15 => Self::LdDt(x),
                0xF if last_byte == 0x18 => Self::LdSt(x),
                0xF if last_byte == 0x1E => Self::AddI(x),
                0xF if last_byte == 0x29 => Self::LdF(x),
                0xF if last_byte == 0x33 => Self::LdB(x),
                0xF if last_byte == 0x55 => Self::LdContToMem(x),
                0xF if last_byte == 0x65 => Self::LdContFromMem(x),
                _ => return None,
            }
        };

        Some(instr)
    }
}
