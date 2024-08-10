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
