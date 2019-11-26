use crate::memory::AddressSpace;
use super::*;

pub(super) fn read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

pub(super) fn modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

pub(super) fn write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}
