use crate::cpu::debug::CpuState;
use crate::ppu::debug::PpuState;
use crate::memory::AddressSpace;

pub struct ClockState {
    pub cpu_cycles: u64,
    pub ppu_cycles: u64,
}
// TODO: expose address spaces (read only?)
pub struct NesState<'a> {
    pub cpu: CpuState,
    pub ppu: PpuState,
    pub clocks: ClockState,

    pub cpu_bus: &'a dyn AddressSpace,
    pub ram: &'a dyn AddressSpace,
    pub vram: &'a dyn AddressSpace,
    pub prg_rom: &'a dyn AddressSpace,
    pub chr_rom: &'a dyn AddressSpace,
}

impl<'a> NesState<'a> {
    pub fn new(nes: &'a super::Nes) -> Self {
        NesState {
            cpu: CpuState::new(&nes.cpu),
            ppu: PpuState::new(&nes.ppu),
            clocks: ClockState {
                cpu_cycles: nes.clocks.cpu_cycles.get(),
                ppu_cycles: nes.clocks.ppu_cycles.get(),
            },

            cpu_bus: &nes.cpu.bus,
            ram: &nes.cpu.bus.ram,
            vram: &nes.cpu.bus.ppu_registers.bus.vram,
            prg_rom: &nes.cpu.bus, // TODO: use mapper directly
            chr_rom: &nes.cpu.bus.ppu_registers.bus, // TODO: use mapper directly
        }
    }

}
