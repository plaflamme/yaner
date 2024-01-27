use std::{collections::VecDeque, pin::Pin, time::SystemTime};

use eframe::{
    egui::{load::SizedTexture, TextureOptions},
    epaint::{ColorImage, ImageData, TextureHandle},
    run_native, App,
};
use fast_image_resize as fr;
use yaner::{cartridge::Cartridge, nes::Stepper};

struct Yaner {
    stepper: Option<Pin<Box<Stepper>>>,

    texture_handle: TextureHandle,

    frames: VecDeque<SystemTime>,
}

impl Yaner {
    fn new(texture_handle: TextureHandle) -> Self {
        Self {
            stepper: None,
            texture_handle,
            frames: VecDeque::with_capacity(1000),
        }
    }

    fn update_ppu_frame(&mut self) {
        if let Some(stepper) = &self.stepper {
            let frame = stepper
                .nes()
                .debug()
                .ppu
                .frame
                .get()
                .iter()
                .flat_map(|pixel| {
                    let (r, g, b) = pixel.rgb();
                    [r, g, b]
                })
                .collect::<Vec<_>>();
            let mut resizer =
                fr::Resizer::new(fr::ResizeAlg::Convolution(fr::FilterType::Lanczos3));

            let factor = 2_usize;
            let src = fr::Image::from_vec_u8(
                std::num::NonZeroU32::new(256).unwrap(),
                std::num::NonZeroU32::new(240).unwrap(),
                frame,
                fr::PixelType::U8x3,
            )
            .unwrap();
            let mut dest = fr::Image::new(
                std::num::NonZeroU32::new(256 * factor as u32).unwrap(),
                std::num::NonZeroU32::new(240 * factor as u32).unwrap(),
                fr::PixelType::U8x3,
            );
            resizer.resize(&src.view(), &mut dest.view_mut()).unwrap();
            self.texture_handle.set(
                ImageData::from(ColorImage::from_rgb(
                    [256 * factor, 240 * factor],
                    dest.buffer(),
                )),
                TextureOptions::default(),
            );
        }
    }

    fn update_frame_rate(&mut self) -> f32 {
        if self.frames.len() == self.frames.capacity() {
            self.frames.pop_front();
        }
        self.frames.push_back(SystemTime::now());

        use itertools::Itertools;
        let frame_rate = self
            .frames
            .iter()
            .tuple_windows()
            .map(|(a, b)| b.duration_since(*a).expect("oops").as_secs_f32())
            .sum::<f32>();

        self.frames.len() as f32 / frame_rate
    }
}

impl App for Yaner {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        eframe::egui::CentralPanel::default().show(ctx, |ui| {
            if ui.button("Open file…").clicked() {
                if let Some(path) = rfd::FileDialog::new().pick_file() {
                    let nes = yaner::nes::Nes::new(Cartridge::try_from(path).unwrap());
                    let stepper = yaner::nes::Stepper::new(nes);
                    self.stepper = Some(stepper);
                }
            } else if let Some(stepper) = &mut self.stepper {
                stepper.step_frame().expect("oops");

                self.update_ppu_frame();
                let frame_rate = self.update_frame_rate();

                ctx.request_repaint();
                ui.image(SizedTexture::from_handle(&self.texture_handle));
                ui.label(format!("{frame_rate}fps"));
            }
        });
    }
}

fn main() -> Result<(), eframe::Error> {
    if let Some(filename) = std::env::args().nth(1) {
        println!("Loading {filename}");
        let native_options = eframe::NativeOptions::default();
        run_native(
            "Yaner",
            native_options,
            Box::new(move |cc| {
                let texture_handle = cc.egui_ctx.load_texture(
                    "frame",
                    eframe::egui::ColorImage::from_rgb([256, 240], &[0; 256 * 240 * 3]),
                    TextureOptions::default(),
                );
                Box::new(Yaner::new(texture_handle))
            }),
        )?;
        Ok(())
    } else {
        panic!("expected filename argument");
    }
}
