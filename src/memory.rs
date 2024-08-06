use std::ops::Range;

use crate::instruction::Address;

pub trait Memory {
    fn get(&self, addr: Address) -> u8;
    fn set(&mut self, addr: Address, value: u8);
    fn range_mut(&mut self, range: Range<usize>) -> &mut [u8];
}

impl<'a, T> Memory for &'a mut T
where
    T: Memory,
{
    fn get(&self, addr: Address) -> u8 {
        <T as Memory>::get(self, addr)
    }

    fn set(&mut self, addr: Address, value: u8) {
        <T as Memory>::set(self, addr, value);
    }

    fn range_mut(&mut self, range: Range<usize>) -> &mut [u8] {
        <T as Memory>::range_mut(self, range)
    }
}

impl Memory for [u8; 4096] {
    fn get(&self, addr: Address) -> u8 {
        self[addr.address()]
    }

    fn set(&mut self, addr: Address, value: u8) {
        self[addr.address()] = value;
    }

    fn range_mut(&mut self, range: Range<usize>) -> &mut [u8] {
        &mut self[range]
    }
}
