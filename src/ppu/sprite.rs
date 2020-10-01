use bitregions::bitregions;

bitregions! {
    pub Attributes u8 {
        PALETTE: 0b0000_0011,
        PRIORITY: 0b0010_0000, // Priority (0: in front of background; 1: behind background)
        FLIP_H: 0b0100_0000,
        FLIP_V: 0b1000_0000,
    }
}

impl Attributes {
    pub fn bg_priority(&self) -> bool {
        self.priority()
    }
    pub fn fg_priority(&self) -> bool {
        !self.priority()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Sprite {
    pub id: u8, // for sprite priority
    pub x: u8,
    pub y: u8,
    pub tile_index: u8,
    pub attr: Attributes,
}

impl Sprite {
    pub fn new(id: u8, values: [u8; 4]) -> Self {
        Sprite {
            id,
            y: values[0],
            tile_index: values[1],
            attr: Attributes::new(values[2]),
            x: values[3],
        }
    }
}

#[derive(Clone, Copy)]
pub struct SpriteData {
    pub sprite: Sprite,
    pub tile_low: u8,
    pub tile_high: u8,
}

impl SpriteData {
    pub fn new(sprite: Sprite, tile_low: u8, tile_high: u8) -> Self {
        SpriteData {
            sprite,
            tile_low,
            tile_high,
        }
    }
}
