// consumes a generator within another generator
#[macro_export]
macro_rules! yield_complete {
    ($co:expr) => {{
        let mut coroutine = $co;
        loop {
            match std::ops::Coroutine::resume(std::pin::Pin::new(&mut coroutine), ()) {
                std::ops::CoroutineState::Yielded(value) => yield value,
                std::ops::CoroutineState::Complete(value) => break value,
            };
        }
    }};
}

#[macro_export]
macro_rules! consume_generator {
    ($co:expr, $y: expr) => {{
        let mut coroutine = $co;
        loop {
            match std::ops::Coroutine::resume(std::pin::Pin::new(&mut coroutine), ()) {
                std::ops::CoroutineState::Yielded(_) => $y,
                std::ops::CoroutineState::Complete(value) => break value,
            };
        }
    }};
}

macro_rules! invalid_address {
    ($addr:expr) => {{
        #[cfg(debug_assertions)]
        {
            panic!("accessed invalid address at 0x{:02X?}", $addr);
        }
    }};
    ($addr:expr, $value:expr) => {{
        #[cfg(debug_assertions)]
        {
            panic!("accessed invalid address at 0x{:02X?}", $addr);
        }
        #[cfg(not(debug_assertions))]
        {
            $value
        }
    }};
}
