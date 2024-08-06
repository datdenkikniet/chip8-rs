#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DisplayCoordinate(u8, u8);

impl DisplayCoordinate {
    pub fn new(x: u8, y: u8) -> Self {
        Self(x, y)
    }

    pub fn x(&self) -> u8 {
        self.0
    }

    pub fn y(&self) -> u8 {
        self.1
    }
}

pub trait Display {
    fn get(&self, coordinate: DisplayCoordinate) -> bool;
    fn set(&mut self, coordinate: DisplayCoordinate, value: bool);
    fn clear_all(&mut self);
}
