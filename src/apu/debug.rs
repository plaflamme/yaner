use crate::apu::Apu;
use crate::apu::Status;
use crate::apu::frame_counter::FrameCounterState;

pub struct ApuState {
    pub status: Status,
    pub frame_counter: FrameCounterState,
}

impl ApuState {
    pub fn new(apu: &Apu) -> Self {
        Self {
            status: apu.status.get(),
            frame_counter: FrameCounterState::new(&apu.frame_counter),
        }
    }
}
