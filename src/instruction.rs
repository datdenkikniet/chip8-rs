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
    pub fn new(value: u8) -> Option<Self> {
        if value > 0xF {
            None
        } else {
            Some(Self(value))
        }
    }

    pub fn get(&self) -> u8 {
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
