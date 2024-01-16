// consumes a generator within another generator
#[macro_export]
macro_rules! yield_complete {
    ($gn:expr) => {{
        let mut gen = $gn;
        loop {
            match std::ops::Coroutine::resume(std::pin::Pin::new(&mut gen), ()) {
                std::ops::CoroutineState::Yielded(value) => yield value,
                std::ops::CoroutineState::Complete(value) => break value,
            };
        }
    }};
}

#[macro_export]
macro_rules! consume_generator {
    ($gn:expr, $y: expr) => {{
        let mut gen = $gn;
        loop {
            match std::ops::Coroutine::resume(std::pin::Pin::new(&mut gen), ()) {
                std::ops::CoroutineState::Yielded(_) => $y,
                std::ops::CoroutineState::Complete(value) => break value,
            };
        }
    }};
}

macro_rules! invalid_address {
    ($addr:expr) => {{
        panic!("accessed invalid address at 0x{:02X?}", $addr);
    }};
    ($addr:expr, $reason:literal) => {{
        panic!("accessed invalid address at 0x{:02X?} : {}", $addr, $reason);
    }};
}
