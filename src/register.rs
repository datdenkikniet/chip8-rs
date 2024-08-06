use crate::{
    instruction::{Address, Nibble},
    Error,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Register {
    V0 = 0,
    V1 = 1,
    V2 = 2,
    V3 = 3,
    V4 = 4,
    V5 = 5,
    V6 = 6,
    V7 = 7,
    V8 = 8,
    V9 = 9,
    VA = 10,
    VB = 11,
    VC = 12,
    VD = 13,
    VE = 14,
    VF = 15,
    ST = 16,
    DT = 17,
}

impl Register {
    pub fn value(&self) -> u16 {
        *self as u16
    }
}

impl From<Register> for usize {
    fn from(value: Register) -> Self {
        value as usize
    }
}

impl From<Nibble> for Register {
    fn from(value: Nibble) -> Self {
        match value.get() {
            0x0 => Self::V0,
            0x1 => Self::V1,
            0x2 => Self::V2,
            0x3 => Self::V3,
            0x4 => Self::V4,
            0x5 => Self::V5,
            0x6 => Self::V6,
            0x7 => Self::V7,
            0x8 => Self::V8,
            0x9 => Self::V9,
            0xA => Self::VA,
            0xB => Self::VB,
            0xC => Self::VC,
            0xD => Self::VD,
            0xE => Self::VE,
            0xF => Self::VF,
            _ => unreachable!(),
        }
    }
}

impl TryFrom<u8> for Register {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let value = Nibble::new(value).ok_or(())?;
        Ok(Self::from(value))
    }
}

#[derive(Default, Debug)]
pub struct RegisterFile {
    registers: [u8; 18],
    pub i: Address,
    pub pc: Address,
    pub sp: u8,
}

impl RegisterFile {
    pub fn get(&self, addr: Register) -> u8 {
        self.registers[usize::from(addr)]
    }

    pub fn set(&mut self, addr: Register, value: u8) {
        self.registers[addr as usize] = value;
    }

    pub fn set_pc(&mut self, pc: Address) {
        self.pc = pc;
    }

    pub fn set_pc_from(&mut self, pc: u16) {
        self.pc = Address::new(pc & 0xFFF).unwrap();
    }

    pub fn inc_pc(&mut self) -> Result<(), Error> {
        if self.pc.get() != 0xFFF {
            self.pc.add(2);
            Ok(())
        } else {
            Err(Error::PcOverflow)
        }
    }

    pub fn pc(&self) -> Address {
        self.pc
    }
}
