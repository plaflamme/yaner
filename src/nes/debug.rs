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

    // TODO: handle different arg sizes (0,1,2) by looking at AddressingMode
    pub fn active_op(&self) -> (OpCode, u16) {
        let op_code = OpCode::decode(self.prg_rom.read_u8(self.cpu.active_pc));
        let arg = self.prg_rom.read_u16(self.cpu.active_pc.saturating_add(1));
        (op_code, arg)
    }
}
