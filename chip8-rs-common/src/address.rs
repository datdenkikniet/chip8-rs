
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