use bitregions::bitregions;

bitregions! {
    pub Attributes u8 {
        PALETTE: 0b0000_0011,
        PRIORITY: 0b0010_0000,
        FLIP_H: 0b0100_0000,
        FLIP_V: 0b1000_0000,
    }
}

#[derive(Clone, Copy)]
pub struct Sprite {
    pub x: u8,
    pub y: u8,
    pub index: u8,
    pub attr: Attributes,
}

impl Sprite {
    pub fn new(values: [u8;4]) -> Self {
       Sprite {
           y: values[0],
           index: values[1],
           attr: Attributes::new(values[2]),
           x: values[3],
       }
    }
}
