// consumes a generator within another generator
#[macro_export]
macro_rules! yield_complete {
    ($gn:expr) => {
        {
            let mut gen = $gn;
            loop {
                match std::ops::Generator::resume(std::pin::Pin::new(&mut gen), ()) {
                    std::ops::GeneratorState::Yielded(value) => yield value,
                    std::ops::GeneratorState::Complete(value) => break value
                };
            }
        }
    }
}

#[macro_export]
macro_rules! consume_generator {
($gn:expr, $y: expr) => {
        {
            let mut gen = $gn;
            loop {
                match std::ops::Generator::resume(std::pin::Pin::new(&mut gen), ()) {
                    std::ops::GeneratorState::Yielded(_) => $y,
                    std::ops::GeneratorState::Complete(value) => break value
                };
            }
        }
    }
}

macro_rules! invalid_address {
    ($addr:expr) => {
        {
            panic!("accessed invalid address at 0x{:02X?}", $addr);
        }
    }
}
