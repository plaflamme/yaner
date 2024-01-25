use std::{collections::VecDeque, pin::Pin, time::SystemTime};

use eframe::{
    egui::{load::SizedTexture, TextureOptions},
    epaint::{ColorImage, ImageData, TextureHandle},
    run_native, App,
};
use yaner::{cartridge::Cartridge, nes::Stepper};

struct Yaner {
    stepper: Pin<Box<Stepper>>,
    texture_handle: TextureHandle,

    frames: VecDeque<SystemTime>,
}

impl Yaner {
    fn new(filename: &str, texture_handle: TextureHandle) -> Self {
        let nes =
            yaner::nes::Nes::new(Cartridge::try_from(std::path::PathBuf::from(filename)).unwrap());
        let stepper = yaner::nes::Stepper::new(nes);
        Self {
            stepper,
            texture_handle,
            frames: VecDeque::with_capacity(1000),
        }
    }
}

impl App for Yaner {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        eframe::egui::CentralPanel::default().show(ctx, |ui| {
            self.stepper.step_frame().expect("oops");
            let frame = self
                .stepper
                .nes()
                .debug()
                .ppu
                .frame
                .get()
                .iter()
                .flat_map(|pixel| {
                    let (r, g, b) = pixel.rgb();
                    vec![r, g, b]
                })
                .collect::<Vec<_>>();

            self.texture_handle.set(
                ImageData::from(ColorImage::from_rgb([256, 240], &frame)),
                TextureOptions::default(),
            );

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

            let frame_rate = self.frames.len() as f32 / frame_rate;

            ctx.request_repaint();
            ui.image(SizedTexture::from_handle(&self.texture_handle));
            ui.label(format!("{frame_rate}fps"));
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
                Box::new(Yaner::new(&filename, texture_handle))
            }),
        )?;
        Ok(())
    } else {
        panic!("expected filename argument");
    }
}
