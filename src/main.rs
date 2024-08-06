use std::{cell::RefCell, io::Read};

use chip8_rs::{Address, Chip8, Display, DisplayCoordinate, Error, Instruction};

fn main() {
    if std::env::args().count() == 1 {
        let instr = &[0xA0, 0x05, 0xD0, 0x05, 0xA0, 0x00, 0xD0, 0x05, 0x00, 0xFD];
        exec(&instr[..]);
    } else {
        for path in std::env::args().skip(1) {
            let mut file = std::fs::File::open(&path).unwrap();

            let mut buffer = Vec::new();
            file.read_to_end(&mut buffer).unwrap();

            exec(&buffer);
        }
    }
}

fn exec(buffer: &[u8]) {
    let display = FakeDisplay::new();
    let mut vm = Chip8::new([0u8; 4096], &display, &buffer);

    loop {
        let res = vm.tick();

        match res {
            Ok(true) => {}
            Ok(false) => break,
            Err(Error::Decode(r)) => {
                println!("Failed to decode: 0x{r:04X} at 0x{:04X}", vm.pc());
                break;
            }
            Err(Error::PcOverflow) => break,
            Err(e) => {
                println!("Failed: {e:?}.");
                break;
            }
        }

        display.draw();

        if vm.decode_current_instr() == Ok(Instruction::Jp(Address::new_truncate(536))) {
            break;
        }

        std::thread::sleep(std::time::Duration::from_millis(10));
    }
}

struct FakeDisplay {
    data: RefCell<[bool; 64 * 32]>,
}

impl FakeDisplay {
    pub fn new() -> Self {
        Self {
            data: RefCell::new([false; 64 * 32]),
        }
    }

    fn index(coordinate: DisplayCoordinate) -> Option<usize> {
        let index = coordinate.x() as usize * 32 + coordinate.y() as usize;

        if index < 2048 {
            Some(index)
        } else {
            None
        }
    }

    pub fn draw(&self) {
        for y in 0..32 {
            for x in 0..64 {
                let coordinate = DisplayCoordinate::new(x, y);
                if self.data.borrow()[Self::index(coordinate).unwrap()] {
                    print!("█");
                } else {
                    print!(" ");
                }
            }

            println!()
        }
    }
}

impl Display for &FakeDisplay {
    fn set(&mut self, coordinate: DisplayCoordinate, value: bool) {
        if let Some(index) = FakeDisplay::index(coordinate) {
            self.data.borrow_mut()[index] = value;
        }
    }

    fn get(&self, coordinate: DisplayCoordinate) -> bool {
        if let Some(index) = FakeDisplay::index(coordinate) {
            self.data.borrow_mut()[index]
        } else {
            false
        }
    }

    fn clear_all(&mut self) {
        *self.data.borrow_mut() = [false; 64 * 32];
    }
}
