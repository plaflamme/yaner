use crate::memory::AddressSpace;
use super::*;

#[allow(dead_code)]
pub(super) fn x_read<'a, O: ReadOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

#[allow(dead_code)]
pub(super) fn y_read<'a, O: ReadOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

#[allow(dead_code)]
pub(super) fn x_modify<'a, O: ModifyOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

#[allow(dead_code)]
pub(super) fn y_modify<'a, O: ModifyOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

#[allow(dead_code)]
pub(super) fn x_write<'a, O: WriteOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

#[allow(dead_code)]
pub(super) fn y_write<'a, O: WriteOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}
