pub struct CpuState {
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub flags: super::Flags,
    pub sp: u8,
    pub intr: Option<super::Interrupt>,
}

impl CpuState {
    pub fn new(cpu: &super::Cpu) -> Self {
        CpuState {
            pc: cpu.pc.get(),
            a: cpu.acc.get(),
            x: cpu.x.get(),
            y: cpu.y.get(),
            sp: cpu.sp.get(),
            flags: cpu.flags.get(),
            intr: cpu.bus.intr.get(),
        }
    }
}
