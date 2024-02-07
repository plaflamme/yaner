use std::ops::Coroutine;

mod frame_counter;

use frame_counter::FrameCounter;

pub enum ApuCycle {
    Tick,
}

pub struct Apu {
    frame_counter: FrameCounter,
}

impl Apu {
    pub fn new() -> Self {
        Self {
            frame_counter: FrameCounter::new(),
        }
    }
    pub fn run(&self) -> impl Coroutine<Yield = ApuCycle, Return = ()> + '_ {
        #[coroutine]
        move || loop {
            self.frame_counter.tick();
            yield ApuCycle::Tick
        }
    }
}
