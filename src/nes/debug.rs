use crate::cpu::debug::CpuState;
use crate::ppu::debug::PpuState;

pub struct ClockState {
    pub cpu_cycles: u64,
    pub ppu_cycles: u64,
}
// TODO: expose address spaces (read only?)
pub struct NesState {
    pub cpu: CpuState,
    pub ppu: PpuState,
    pub clocks: ClockState,
}

impl NesState {
    pub fn new(nes: &super::Nes) -> Self {
        NesState {
            cpu: CpuState::new(&nes.cpu),
            ppu: PpuState::new(&nes.ppu),
            clocks: ClockState {
                cpu_cycles: nes.clocks.cpu_cycles.get(),
                ppu_cycles: nes.clocks.ppu_cycles.get(),
            },
        }
    }

}
