#![allow(clippy::zero_ptr)]

use crate::cartridge::Cartridge;
use crate::input::JoypadButtons;
use crate::nes::{Nes, Stepper};
use crate::ppu::PpuCycle;
use crate::Reset;
use libretro_backend::{
    AudioVideoInfo, Core, CoreInfo, GameData, JoypadButton, LoadGameResult, PixelFormat, Region,
    RuntimeHandle,
};
use std::convert::TryFrom;
use std::path::PathBuf;
use std::pin::Pin;

#[derive(Default)]
struct YanerCore {
    stepper: Option<Pin<Box<Stepper>>>,
    game_data: Option<GameData>,
}

impl Core for YanerCore {
    fn info() -> CoreInfo {
        CoreInfo::new("Yaner", env!("CARGO_PKG_VERSION")).supports_roms_with_extension("nes")
    }

    fn on_load_game(&mut self, game_data: GameData) -> LoadGameResult {
        let cartridge = match (game_data.data(), game_data.path()) {
            (Some(data), _) => Cartridge::try_from(data),
            (_, Some(path)) => Cartridge::try_from(PathBuf::from(path)),
            _ => return LoadGameResult::Failed(game_data),
        };

        match cartridge {
            Err(_) => LoadGameResult::Failed(game_data),
            Ok(cartridge) => {
                self.stepper = Some(Stepper::new(Nes::new(cartridge)));
                self.game_data = Some(game_data);
                let av_info = AudioVideoInfo::new()
                    .video(256, 240, 60.0, PixelFormat::ARGB8888)
                    .audio(44100.0)
                    .region(Region::NTSC);

                LoadGameResult::Success(av_info)
            }
        }
    }

    fn on_unload_game(&mut self) -> GameData {
        // TODO: what is this supposed to return?
        self.game_data.take().unwrap()
    }

    fn on_run(&mut self, handle: &mut RuntimeHandle) {
        macro_rules! extract_buttons {
            ($id:expr, $($btn:ident),+) => {{
                let mut buttons = JoypadButtons::empty();
                $(
                    if handle.is_joypad_button_pressed($id, JoypadButton::$btn) {
                        buttons |= JoypadButtons::$btn;
                    }
                )+
                buttons
            }}
        }
        if let Some(stepper) = self.stepper.as_mut() {
            stepper.nes().input1.update(extract_buttons!(
                0, A, B, Start, Select, Up, Down, Left, Right
            ));
            stepper.nes().input2.update(extract_buttons!(
                1, A, B, Start, Select, Up, Down, Left, Right
            ));
            match stepper.step_frame().unwrap() {
                PpuCycle::Frame => {
                    let state = stepper.nes().debug();
                    let frame = state
                        .ppu
                        .frame
                        .iter()
                        .flat_map(|pixel| {
                            let (r, g, b) = pixel.rgb();
                            vec![b, g, r, 0]
                        })
                        .collect::<Vec<_>>();
                    handle.upload_video_frame(frame.as_slice());
                    handle.upload_audio_frame(&[0; 1470]);
                }
                y => panic!("unexpected value yielded by NES: {:?}", y),
            }
        }
    }

    fn on_reset(&mut self) {
        if let Some(stepper) = &self.stepper {
            stepper.reset()
        }
    }
}

libretro_backend::libretro_core!(YanerCore);
