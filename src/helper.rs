// consumes a generator within another generator
#[macro_export]
macro_rules! yield_complete {
    ($gn:expr) => {
        {
            use std::ops::{Generator, GeneratorState};
            use std::pin::Pin;
            let mut gen = $gn;
            loop {
                match Generator::resume(Pin::new(&mut gen)) {
                    GeneratorState::Yielded(value) => yield value,
                    GeneratorState::Complete(value) => break value
                };
            }
        }
    }
}
