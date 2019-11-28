use crate::memory::AddressSpace;
use super::*;

#[allow(dead_code)]
pub(super) fn read<'a, O: ReadOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

#[allow(dead_code)]
pub(super) fn modify<'a, O: ModifyOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

#[allow(dead_code)]
pub(super) fn write<'a, O: WriteOperation>(_operation: &'a O, _cpu: &'a Cpu, _mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}
