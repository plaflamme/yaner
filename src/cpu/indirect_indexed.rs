use crate::memory::AddressSpace;
use super::*;

pub(super) fn x_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

pub(super) fn y_read<'a, O: ReadOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

pub(super) fn x_modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

pub(super) fn y_modify<'a, O: ModifyOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

pub(super) fn x_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}

pub(super) fn y_write<'a, O: WriteOperation>(operation: &'a O, cpu: &'a Cpu, mem_map: &'a Box<&dyn AddressSpace>) -> impl Generator<Yield = CpuCycle, Return = ()> + 'a {
    move || {
        yield CpuCycle::Tick;
        unimplemented!()
    }
}
