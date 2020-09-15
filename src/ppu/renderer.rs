use std::cell::Cell;

#[derive(Clone, Default)]
pub struct RegisterPair<T: Copy> {
    pub low: Cell<T>,
    pub high: Cell<T>,
}

#[derive(Clone, Default)]
pub struct PatternData {
    pub latch: RegisterPair<u8>,
    pub value: RegisterPair<u16>
}

impl PatternData {
    pub fn latch(&self) {
        self.value.low.update(|v| (v & 0xFF00) | self.latch.low.get() as u16);
        self.value.high.update(|v| (v & 0xFF00) | self.latch.high.get() as u16);
    }

    pub fn shift(&self) {
        self.value.low.update(|v| v << 1);
        self.value.high.update(|v| v << 1);
    }
}

#[derive(Clone, Default)]
pub struct AttributeData {
    pub latch: RegisterPair<u8>, // this is actually a 1bit latch
    pub value: RegisterPair<u8>
}

impl AttributeData {

    pub fn latch(&self) {
        self.value.low.update(|v| (v & 0xFE) | self.latch.low.get() & 1);
        self.value.high.update(|v| (v & 0xFE) | self.latch.high.get() & 1);
    }

    pub fn shift(&self) {
        self.value.low.update(|v| v << 1 | self.latch.low.get());
        self.value.high.update(|v| v << 1 | self.latch.high.get());
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_pattern_data_latch() {
        let pd = PatternData::default();
        pd.latch();
        assert_eq!(pd.value.low.get(), 0);
        assert_eq!(pd.value.high.get(), 0);

        pd.latch.low.set(0xAA);
        pd.latch.high.set(0xBB);
        pd.latch();
        assert_eq!(pd.value.low.get(), 0x00AA);
        assert_eq!(pd.value.high.get(), 0x00BB);

        pd.value.low.set(0x1100);
        pd.value.high.set(0x2200);
        pd.latch();
        assert_eq!(pd.value.low.get(), 0x11AA);
        assert_eq!(pd.value.high.get(), 0x22BB);
    }

    #[test]
    fn test_pattern_data_shift() {
        let pd = PatternData::default();
        pd.shift();
        assert_eq!(pd.value.low.get(), 0);
        assert_eq!(pd.value.high.get(), 0);

        pd.value.low.set(0b0101_0101_0101_0101);
        pd.value.high.set(0b1010_1010_1010_1010);
        pd.shift();
        assert_eq!(pd.value.low.get(), 0b1010_1010_1010_1010);
        assert_eq!(pd.value.high.get(), 0b0101_0101_0101_0100);
        pd.shift();
        assert_eq!(pd.value.low.get(), 0b0101_0101_0101_0100);
        assert_eq!(pd.value.high.get(), 0b1010_1010_1010_1000);
    }

    #[test]
    fn test_attribute_data_latch() {
        let ad = AttributeData::default();
        ad.latch();
        assert_eq!(ad.value.low.get(), 0);
        assert_eq!(ad.value.high.get(), 0);

        ad.latch.low.set(0x01);
        ad.latch.high.set(0x01);
        ad.latch();
        assert_eq!(ad.value.low.get(), 0x01);
        assert_eq!(ad.value.high.get(), 0x01);

        ad.value.low.set(0b1111_1110);
        ad.value.high.set(0b1111_1110);
        ad.latch();
        assert_eq!(ad.value.low.get(), 0b1111_1111);
        assert_eq!(ad.value.high.get(), 0b1111_1111);

        ad.value.low.set(0b0000_0000);
        ad.value.high.set(0b0000_0000);
        ad.latch();
        assert_eq!(ad.value.low.get(), 0b0000_0001);
        assert_eq!(ad.value.high.get(), 0b0000_0001);
    }

    #[test]
    fn test_attribute_data_shift() {
        let ad = AttributeData::default();
        ad.shift();
        assert_eq!(ad.value.low.get(), 0);
        assert_eq!(ad.value.high.get(), 0);

        ad.value.low.set(0b0101_0101);
        ad.value.high.set(0b1010_1010);
        ad.shift();
        assert_eq!(ad.value.low.get(), 0b1010_1010);
        assert_eq!(ad.value.high.get(), 0b0101_0100);
        ad.shift();
        assert_eq!(ad.value.low.get(), 0b0101_0100);
        assert_eq!(ad.value.high.get(), 0b1010_1000);
    }

}
