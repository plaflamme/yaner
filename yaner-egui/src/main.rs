use std::pin::Pin;

use eframe::{run_native, App};
use yaner::{cartridge::Cartridge, nes::Stepper};

struct Yaner {
    stepper: Pin<Box<Stepper>>,
}

impl Yaner {
    fn new(filename: &str) -> Self {
        let nes =
            yaner::nes::Nes::new(Cartridge::try_from(std::path::PathBuf::from(filename)).unwrap());
        let stepper = yaner::nes::Stepper::new(nes);
        Self { stepper }
    }
}

impl App for Yaner {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        eframe::egui::CentralPanel::default().show(ctx, |ui| ui.label("Hello Yanner"));
    }
}

fn main() -> Result<(), eframe::Error> {
    if let Some(filename) = std::env::args().nth(1) {
        println!("Loading {filename}");
        let native_options = eframe::NativeOptions::default();
        run_native(
            "Yaner",
            native_options,
            Box::new(move |cc| Box::new(Yaner::new(&filename))),
        )?;
        Ok(())
    } else {
        panic!("expected filename argument");
    }
}
