use std::{collections::VecDeque, pin::Pin, time::SystemTime};

use eframe::{
    egui::{load::SizedTexture, TextureOptions},
    epaint::{ColorImage, ImageData, TextureHandle},
    run_native, App,
};
use fast_image_resize as fr;
use yaner::{cartridge::Cartridge, nes::Stepper};

const NTSC_HEIGHT: u32 = 256;
const NTSC_WIDTH: u32 = 240;

struct Settings {
    image_size_factor: u32,
}

struct Yaner {
    stepper: Option<Pin<Box<Stepper>>>,

    texture_handle: TextureHandle,

    frames: VecDeque<SystemTime>,

    settings: Settings,
}

impl Yaner {
    fn new(texture_handle: TextureHandle) -> Self {
        Self {
            stepper: None,
            texture_handle,
            frames: VecDeque::with_capacity(1000),
            settings: Settings {
                image_size_factor: 1,
            },
        }
    }

    fn update_ppu_frame(&mut self) -> Option<(u32, u32)> {
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

            let factor = self.settings.image_size_factor;
            let src = fr::Image::from_vec_u8(
                std::num::NonZeroU32::new(NTSC_HEIGHT).unwrap(),
                std::num::NonZeroU32::new(NTSC_WIDTH).unwrap(),
                frame,
                fr::PixelType::U8x3,
            )
            .unwrap();
            let mut dest = fr::Image::new(
                std::num::NonZeroU32::new(NTSC_HEIGHT * factor).unwrap(),
                std::num::NonZeroU32::new(NTSC_WIDTH * factor as u32).unwrap(),
                fr::PixelType::U8x3,
            );
            resizer.resize(&src.view(), &mut dest.view_mut()).unwrap();
            self.texture_handle.set(
                ImageData::from(ColorImage::from_rgb(
                    [
                        (NTSC_HEIGHT * factor) as usize,
                        (NTSC_WIDTH * factor) as usize,
                    ],
                    dest.buffer(),
                )),
                TextureOptions::default(),
            );
            Some((NTSC_HEIGHT * factor, NTSC_WIDTH * factor))
        } else {
            None
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
            .map(|(a, b)| {
                b.duration_since(*a)
                    .expect("previous frame expected to be in the past")
                    .as_secs_f32()
            })
            .sum::<f32>();

        self.frames.len() as f32 / frame_rate
    }
}

impl App for Yaner {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        eframe::egui::TopBottomPanel::top("menubar").show(ctx, |ui| {
            eframe::egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Open").clicked() {
                        if let Some(path) = rfd::FileDialog::new().pick_file() {
                            let nes = yaner::nes::Nes::new(Cartridge::try_from(path).unwrap());
                            let stepper = yaner::nes::Stepper::new(nes);
                            self.stepper = Some(stepper);
                        }
                        ui.close_menu();
                    }
                    if ui.button("Exit").clicked() {
                        ui.close_menu();
                        std::process::exit(0);
                    }
                });
                ui.menu_button("Edit", |ui| {
                    let a = ui
                        .radio_value(&mut self.settings.image_size_factor, 1, "1x")
                        .clicked();
                    let b = ui
                        .radio_value(&mut self.settings.image_size_factor, 2, "2x")
                        .clicked();
                    let c = ui
                        .radio_value(&mut self.settings.image_size_factor, 3, "3x")
                        .clicked();
                    if a || b || c {
                        ui.close_menu();
                    }
                });
            })
        });

        eframe::egui::CentralPanel::default().show(ctx, |ui| {
            if let Some(stepper) = &mut self.stepper {
                stepper.step_frame().expect("oops");

                ui.horizontal(|ui| {});

                let (height, width) = self.update_ppu_frame().unwrap();

                ctx.request_repaint();
                ui.image(SizedTexture::from_handle(&self.texture_handle));
            }
        });

        eframe::egui::TopBottomPanel::bottom("footer").show(ctx, |ui| {
            eframe::egui::menu::bar(ui, |ui| {
                if let Some(_) = &mut self.stepper {
                    let frame_rate = self.update_frame_rate();
                    ui.label(format!("{frame_rate:0.2}fps"));
                }
            })
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
