use yaner_cpu::OpCode;

use crate::memory::AddressSpace;
use crate::ppu::debug::PpuState;

pub struct ClockState {
    pub cpu_master_clock: usize,
    pub ppu_master_clock: usize,
    pub cpu_cycles: u64,
    pub ppu_cycles: u64,
    pub ppu_frames: u64,
}
// TODO: expose address spaces (read only?)
pub struct NesState<'a> {
    pub cpu: yaner_cpu::CpuState,
    pub ppu: PpuState<'a>,
    pub clocks: ClockState,

    pub cpu_bus: &'a dyn AddressSpace,
    pub ppu_bus: &'a dyn AddressSpace,
    pub ram: &'a dyn AddressSpace,
    pub vram: &'a dyn AddressSpace,
    pub prg_rom: &'a dyn AddressSpace,
    pub chr_rom: &'a dyn AddressSpace,
}

impl<'a> NesState<'a> {
    pub fn new(nes: &'a super::Nes) -> Self {
        NesState {
            cpu: yaner_cpu::CpuState::new(&nes.cpu),
            ppu: PpuState::new(&nes.ppu),
            clocks: ClockState {
                cpu_master_clock: nes.clocks.cpu_master_clock.get(),
                ppu_master_clock: nes.clocks.ppu_master_clock.get(),
                cpu_cycles: nes.clocks.cpu_cycles.get(),
                ppu_cycles: nes.clocks.ppu_cycles.get(),
                ppu_frames: nes.clocks.ppu_frames.get(),
            },

            cpu_bus: &nes.cpu_bus,
            ppu_bus: &nes.ppu.bus,
            ram: &nes.cpu_bus.ram,
            vram: &nes.ppu.bus.vram,
            prg_rom: &nes.cpu_bus, // TODO: use mapper directly
            chr_rom: &nes.ppu.bus, // TODO: use mapper directly
        }
    }

    pub fn instruction(&self, addr: u16) -> Instruction {
        let op_code = OpCode::decode(self.prg_rom.read_u8(addr));
        let operand = self.prg_rom.read_u16(self.cpu.active_pc.saturating_add(1));
        Instruction {
            op: op_code.0,
            mode: op_code.1,
            operand,
        }
    }

    pub fn active_op(&self) -> Instruction {
        self.instruction(self.cpu.active_pc)
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub op: yaner_cpu::Op,
    pub mode: yaner_cpu::AddressingMode,
    pub operand: u16,
}

impl Instruction {
    pub fn size(&self) -> u16 {
        match self.mode {
            yaner_cpu::AddressingMode::Imp => 0,
            yaner_cpu::AddressingMode::Acc => 0,
            yaner_cpu::AddressingMode::Imm => 1,
            yaner_cpu::AddressingMode::Zp0 => 1,
            yaner_cpu::AddressingMode::ZpX => 1,
            yaner_cpu::AddressingMode::ZpY => 1,
            yaner_cpu::AddressingMode::Rel => 1,
            yaner_cpu::AddressingMode::Ind => 1,
            yaner_cpu::AddressingMode::IdX => 1,
            yaner_cpu::AddressingMode::IdY => 1,
            yaner_cpu::AddressingMode::Abs => 2,
            yaner_cpu::AddressingMode::AbX => 2,
            yaner_cpu::AddressingMode::AbY => 2,
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use yaner_cpu::AddressingMode;
        write!(f, "{:?}", self.op)?;
        match self.mode {
            AddressingMode::Imp => write!(f, ""),
            AddressingMode::Acc => write!(f, " A"),
            AddressingMode::Imm => write!(f, " #${:02X}", self.operand & 0x00FF),
            AddressingMode::Zp0 => write!(f, " ${:02X}", self.operand & 0x00FF),
            AddressingMode::ZpX => write!(f, " ${:02X},X", self.operand & 0x00FF),
            AddressingMode::ZpY => write!(f, " ${:02X},Y", self.operand & 0x00FF),
            AddressingMode::Rel => write!(f, " +${:02X}", self.operand & 0x00FF),
            AddressingMode::Ind => write!(f, " (${:02X})", self.operand & 0x00FF),
            AddressingMode::IdX => write!(f, " (${:02X},X)", self.operand & 0x00FF),
            AddressingMode::IdY => write!(f, " (${:02X}),Y", self.operand & 0x00FF),
            AddressingMode::Abs => write!(f, " ${:04X}", self.operand),
            AddressingMode::AbX => write!(f, " ${:04X},X", self.operand),
            AddressingMode::AbY => write!(f, " ${:04X},Y", self.operand),
        }
    }
}
