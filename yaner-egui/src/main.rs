use std::{collections::VecDeque, path::PathBuf, time::SystemTime};

use eframe::{
    egui::{load::SizedTexture, FontDefinitions, KeyboardShortcut, Modifiers, TextureOptions},
    epaint::{ColorImage, FontFamily, ImageData, TextureHandle},
    run_native, App, CreationContext,
};
use fast_image_resize as fr;
use yaner::{input::JoypadButtons, nes::Steps, ppu::renderer::Pixel};

mod menubar;

const NES_FRAME_HEIGHT: u32 = 256;
const NES_FRAME_WIDTH: u32 = 240;

struct Settings {
    image_size_factor: u32,
}

struct Yaner {
    stepper: Option<Steps>,

    ppu_frame_handle: TextureHandle,

    frames: VecDeque<SystemTime>,

    settings: Settings,
}

impl Yaner {
    fn new(cc: &CreationContext) -> Self {
        Self {
            stepper: None,
            ppu_frame_handle: cc.egui_ctx.load_texture(
                "frame",
                eframe::egui::ColorImage::from_rgb(
                    [NES_FRAME_HEIGHT as usize, NES_FRAME_WIDTH as usize],
                    &[0; (NES_FRAME_HEIGHT * NES_FRAME_WIDTH * 3) as usize],
                ),
                TextureOptions::default(),
            ),
            frames: VecDeque::with_capacity(1000),
            settings: Settings {
                image_size_factor: 2,
            },
        }
    }

    fn open(&mut self, path: PathBuf) {
        let nes = yaner::nes::Nes::new(yaner::cartridge::Cartridge::try_from(path).unwrap());
        self.stepper = Some(nes.steps());
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

struct Shortcuts {
    open_file: KeyboardShortcut,
    close: KeyboardShortcut,
    reset: KeyboardShortcut,
}

#[cfg(not(target_os = "macos"))]
fn shortcuts() -> Shortcuts {
    Shortcuts {
        open_file: KeyboardShortcut::new(Modifiers::CTRL, eframe::egui::Key::O),
        close: KeyboardShortcut::new(Modifiers::ALT, eframe::egui::Key::F4),
        reset: KeyboardShortcut::new(Modifiers::CTRL, eframe::egui::Key::R),
    }
}

#[cfg(target_os = "macos")]
fn shortcuts() -> Shortcuts {
    Shortcuts {
        open_file: KeyboardShortcut::new(Modifiers::MAC_CMD, eframe::egui::Key::O),
        close: KeyboardShortcut::new(Modifiers::MAC_CMD, eframe::egui::Key::W),
        reset: KeyboardShortcut::new(Modifiers::MAC_CMD, eframe::egui::Key::R),
    }
}

fn ppu_frame_to_image_data(
    frame: &[std::cell::Cell<Pixel>; 256 * 240],
    resize_factor: u32,
) -> ImageData {
    let frame = frame
        .iter()
        .flat_map(|pixel| {
            let (r, g, b) = pixel.get().rgb();
            [r, g, b]
        })
        .collect::<Vec<_>>();
    let mut resizer = fr::Resizer::new(fr::ResizeAlg::Nearest);

    let src = fr::Image::from_vec_u8(
        std::num::NonZeroU32::new(NES_FRAME_HEIGHT).unwrap(),
        std::num::NonZeroU32::new(NES_FRAME_WIDTH).unwrap(),
        frame,
        fr::PixelType::U8x3,
    )
    .unwrap();
    let mut dest = fr::Image::new(
        std::num::NonZeroU32::new(NES_FRAME_HEIGHT * resize_factor).unwrap(),
        std::num::NonZeroU32::new(NES_FRAME_WIDTH * resize_factor).unwrap(),
        fr::PixelType::U8x3,
    );
    resizer.resize(&src.view(), &mut dest.view_mut()).unwrap();

    ImageData::from(ColorImage::from_rgb(
        [
            (NES_FRAME_HEIGHT * resize_factor) as usize,
            (NES_FRAME_WIDTH * resize_factor) as usize,
        ],
        dest.buffer(),
    ))
}

impl App for Yaner {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        let mut font = FontDefinitions::default();
        font.families.get_mut(&FontFamily::Monospace).unwrap();
        ctx.set_fonts(font);

        eframe::egui::TopBottomPanel::top("menubar").show(ctx, |ui| menubar::show(self, ui));

        eframe::egui::TopBottomPanel::bottom("footer").show(ctx, |ui| {
            eframe::egui::menu::bar(ui, |ui| {
                if self.stepper.is_some() {
                    let frame_rate = self.update_frame_rate();
                    ui.label(format!("{frame_rate:0.2}fps"));
                }
            })
        });

        eframe::egui::CentralPanel::default().show(ctx, |ui| {
            ui.centered_and_justified(|ui| {
                if let Some(stepper) = self.stepper.as_mut() {
                    ctx.input(|input| {
                        let mut buttons = JoypadButtons::empty();
                        if input.key_down(eframe::egui::Key::Enter) {
                            buttons |= JoypadButtons::Start;
                        }
                        if input.key_down(eframe::egui::Key::ArrowRight) {
                            buttons |= JoypadButtons::Right;
                        }
                        if input.key_down(eframe::egui::Key::ArrowLeft) {
                            buttons |= JoypadButtons::Left;
                        }
                        if input.key_down(eframe::egui::Key::ArrowUp) {
                            buttons |= JoypadButtons::Up;
                        }
                        if input.key_down(eframe::egui::Key::ArrowDown) {
                            buttons |= JoypadButtons::Down;
                        }
                        if input.key_down(eframe::egui::Key::Z) {
                            buttons |= JoypadButtons::A;
                        }
                        if input.key_down(eframe::egui::Key::X) {
                            buttons |= JoypadButtons::B;
                        }

                        stepper.nes().input1.update(buttons);
                    });
                    stepper.step_frame().expect("oops");
                    let frame = stepper.current_frame();
                    let image_data =
                        ppu_frame_to_image_data(frame, self.settings.image_size_factor);
                    self.ppu_frame_handle
                        .set(image_data, TextureOptions::default());
                    ui.image(SizedTexture::from_handle(&self.ppu_frame_handle));
                }
            })
        });

        if self.stepper.is_some() {
            ctx.request_repaint();
        }
    }
}

fn main() -> Result<(), eframe::Error> {
    let native_options = eframe::NativeOptions::default();
    run_native(
        "Yaner",
        native_options,
        Box::new(move |cc| Box::new(Yaner::new(cc))),
    )?;
    Ok(())
}
