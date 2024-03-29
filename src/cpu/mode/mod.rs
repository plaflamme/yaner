use super::*;

// timing information is from here http://nesdev.com/6502_cpu.txt

pub mod absolute;
pub mod absolute_indexed;
pub mod accumulator;
pub mod immediate;
pub mod implicit;
pub mod indirect;
pub mod indirect_indexed;
pub mod relative;
pub mod stack;
pub mod zero_page;
pub mod zero_page_indexed;
