mod data;
mod display;
mod instruction;
mod memory;
mod register;

pub use data::{Address, Nibble};
pub use display::{Display, DisplayCoordinate};
pub use instruction::Instruction;
pub use memory::Memory;
pub use register::Register;

use register::RegisterFile;

#[derive(Debug, PartialEq)]
pub enum Error {
    StackOverflow,
    StackUnderflow,
    SysInstr,
    Decode(u16),
    PcOverflow,
}

#[derive(Debug)]
pub struct Chip8<Mem, Display> {
    memory: Mem,
    display: Display,
    registers: RegisterFile,
    rng_state: (u32, u32),
}

impl<MEM, DISPLAY> Chip8<MEM, DISPLAY> {
    fn init_memory(memory: &mut impl Memory) {
        let interpreter_memory = include_bytes!("./interpreter_mem.bin");
        memory
            .range_mut(0..interpreter_memory.len())
            .copy_from_slice(interpreter_memory);
    }
}

impl<Mem, Display> Chip8<Mem, Display>
where
    Mem: Memory,
{
    pub fn seed_rng(&mut self, seed: (u32, u32)) {
        self.rng_state = seed;
    }

    pub fn new_skip_init(pc: Address, memory: Mem, display: Display) -> Self {
        let mut registers = RegisterFile::default();
        registers.set_pc(pc);

        Self {
            memory,
            display,
            registers,
            rng_state: (1, 1),
        }
    }

    pub fn new(mut memory: Mem, display: Display, program: &[u8]) -> Self {
        Self::init_memory(&mut memory);

        let program_data = program.as_ref();

        memory
            .range_mut(0x200..0x200 + program_data.len())
            .copy_from_slice(program_data);

        Self::new_skip_init(Address::new_truncate(0x200), memory, display)
    }

    pub fn pc(&self) -> Address {
        self.registers.pc()
    }

    fn next_rand(state: &mut (u32, u32)) -> u8 {
        let (v, u) = state;
        *v = 36969 * (*v & 65535) + (*v >> 16);
        *u = 18000 * (*u & 65535) + (*u >> 16);

        if *u == 0 {
            *u = 1;
        }

        if *v == 0 {
            *v = 1;
        }

        return *u as u8;
    }
}

impl<Mem, Display> Chip8<Mem, Display>
where
    Display: crate::Display,
    Mem: Memory,
{
    fn execute(&mut self, instruction: Instruction) -> Result<bool, Error> {
        eprintln!("{:?}", instruction);

        let Self {
            memory,
            display,
            registers,
            rng_state,
        } = self;

        match instruction {
            Instruction::Sys { .. } => return Err(Error::SysInstr),
            Instruction::Cls => display.clear_all(),
            Instruction::Ret => {
                if registers.sp == 0x00 {
                    return Err(Error::StackUnderflow);
                }

                let sp = registers.sp as usize * 2;
                let pc = u16::from_ne_bytes(
                    memory
                        .range_mut(sp..sp + 2)
                        .try_into()
                        .expect("Always works"),
                );
                registers.sp -= 1;
                registers.set_pc_from(pc);
                return Ok(true);
            }
            Instruction::Jp { address: addr } => {
                registers.set_pc(addr);
                return Ok(true);
            }
            Instruction::Call { address: addr } => {
                if registers.sp == 0xF {
                    return Err(Error::StackOverflow);
                }

                let sp = registers.sp as usize * 2;

                memory
                    .range_mut(sp..sp + 2)
                    .copy_from_slice(&registers.pc().get().to_ne_bytes());

                registers.sp += 1;
                registers.set_pc(addr);
                return Ok(true);
            }
            Instruction::SeVal { x, k } => {
                if registers.get(x) == k {
                    registers.inc_pc()?;
                }
            }
            Instruction::SneVal { x, k } => {
                if registers.get(x) == k {
                    registers.inc_pc()?;
                }
            }
            Instruction::SeReg { x, y } => {
                if registers.get(x) == registers.get(y) {
                    registers.inc_pc()?;
                }
            }
            Instruction::LdVal { x, k } => registers.set(x, k),
            Instruction::AddVal { x, k } => {
                let v = registers.get(x);
                registers.set(x, k.wrapping_add(v));
            }
            Instruction::LdReg { x, y } => {
                let v = registers.get(y);
                registers.set(x, v);
            }
            Instruction::Or { x, y } => {
                let v1 = registers.get(y);
                let v2 = registers.get(x);
                registers.set(x, v1 | v2);
            }
            Instruction::And { x, y } => {
                let v1 = registers.get(y);
                let v2 = registers.get(x);
                registers.set(x, v1 & v2);
            }
            Instruction::Xor { x, y } => {
                let v1 = registers.get(y);
                let v2 = registers.get(x);
                registers.set(x, v1 ^ v2);
            }
            Instruction::AddReg { x, y } => {
                let v1 = registers.get(y);
                let v2 = registers.get(x);
                registers.set(x, v1.wrapping_add(v2));
                registers.set(Register::VF, v1.checked_add(v2).is_none() as u8);
            }
            Instruction::Sub { x, y } => {
                let v1 = registers.get(y);
                let v2 = registers.get(x);
                registers.set(x, v2.wrapping_sub(v1));
                registers.set(Register::VF, v2.checked_sub(v1).is_none() as u8);
            }
            Instruction::Shr { x, y } => {
                let v = registers.get(y);
                let vf = v & 0x1;
                registers.set(x, v >> 1);
                registers.set(Register::VF, vf);
            }
            Instruction::Subn { x, y } => {
                let v1 = registers.get(y);
                let v2 = registers.get(x);
                registers.set(x, v1.wrapping_sub(v2));
                registers.set(Register::VF, v1.checked_sub(v2).is_none() as u8);
            }
            Instruction::Shl { x, y } => {
                let v = registers.get(y);
                let vf = (v & 0x80) >> 7;
                registers.set(x, v << 1);
                registers.set(Register::VF, vf);
            }
            Instruction::SneReg { x, y } => {
                if registers.get(x) != registers.get(y) {
                    registers.inc_pc()?;
                }
            }
            Instruction::LdI { address: addr } => registers.i = addr,
            Instruction::JpOffset { offset } => {
                let reg_offset = registers.get(Register::V0);
                // TODO: check overflow?
                let pc = offset.get().wrapping_add(reg_offset as u16);

                registers.set_pc(Address::new_truncate(pc));
                return Ok(true);
            }
            Instruction::Rnd { x, k } => {
                let rand = Self::next_rand(rng_state);
                registers.set(x, rand & k);
            }
            Instruction::Drw { x, y, n } => {
                registers.set(Register::VF, 0);

                let mut addr = registers.i;
                let init_x = registers.get(x);
                let init_y = registers.get(y);

                let mut y = init_y;

                for _ in 0..n.get() {
                    let mut value = memory.get(addr);
                    let mut x = init_x;

                    for x_add in 0..8 {
                        let x_offset = x + x_add;
                        let bit = (value & 0x80) == 0x80;
                        value <<= 1;

                        let coordinate = DisplayCoordinate::new(x_offset, y);
                        let value = display.get(coordinate);

                        if bit && value {
                            registers.set(Register::VF, 1);
                        }

                        display.set(coordinate, bit ^ value);

                        x += 1;
                    }

                    y += 1;
                    addr.add(1);
                }
            }
            Instruction::AddI { x } => registers.i.add(registers.get(x)),
            Instruction::Skp { .. } => todo!("skp"),
            Instruction::Sknp { .. } => todo!("sknp"),
            Instruction::LdDtToReg { .. } => todo!("lddttoreg"),
            Instruction::LdKey { .. } => todo!("ldkey"),
            Instruction::LdDt { .. } => todo!("lddt"),
            Instruction::LdSt { .. } => todo!("ldst"),
            Instruction::LdF { .. } => todo!("ldf"),
            Instruction::LdB { .. } => todo!("ldb"),
            Instruction::LdContToMem { .. } => todo!("ldconttomem"),
            Instruction::LdContFromMem { .. } => todo!("ldcontfrommem"),
            Instruction::Exit => return Ok(false),
        }

        registers.inc_pc()?;

        Ok(true)
    }

    pub fn tick(&mut self) -> Result<bool, Error> {
        let instruction = self.decode_current_instr()?;
        self.execute(instruction)
    }
}

impl<Memory, Display> Chip8<Memory, Display>
where
    Memory: crate::Memory,
{
    pub fn decode_current_instr(&self) -> Result<Instruction, Error> {
        let pc = self.registers.pc().get() as usize;
        let instr_data = self.memory.range(pc..pc + 2);
        let instr = u16::from_be_bytes(instr_data.try_into().expect("Always succeeds."));
        Instruction::decode(instr).ok_or(Error::Decode(instr))
    }
}
