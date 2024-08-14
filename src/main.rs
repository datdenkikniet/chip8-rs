use std::{cell::RefCell, io::Read};

use chip8_rs::{Address, Chip8, Display, DisplayCoordinate, Error, Instruction, Nibble};

fn main() {
    if std::env::args().count() == 1 {
        use chip8_rs::Register::*;
        use Instruction::*;

        const FIVE: Nibble = Nibble::new_truncate(5);

        let instr: Vec<_> = [
            LdI {
                address: Address::new_truncate(0 * 5),
            },
            Drw {
                x: V0,
                y: V0,
                n: FIVE,
            },
            LdI {
                address: Address::new_truncate(1 * 5),
            },
            LdVal { x: V0, k: 8 },
            Drw {
                x: V0,
                y: V1,
                n: FIVE,
            },
            Exit,
        ]
        .into_iter()
        .flat_map(|v| v.encode().to_be_bytes())
        .collect();

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

        if vm.decode_current_instr()
            == Ok(Instruction::Jp {
                address: Address::new_truncate(536),
            })
        {
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
