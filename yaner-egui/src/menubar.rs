use eframe::egui::{Button, Ui};
use yaner::cartridge::Cartridge;
use yaner::Reset;

pub(super) fn show(app: &mut super::Yaner, ui: &mut Ui) -> eframe::egui::InnerResponse<()> {
    let shortcuts = super::shortcuts();

    let close = || std::process::exit(0);

    if ui.input_mut(|i| i.consume_shortcut(&shortcuts.open_file)) {
        if let Some(path) = rfd::FileDialog::new().pick_file() {
            let nes = yaner::nes::Nes::new(Cartridge::try_from(path).unwrap());
            app.stepper = Some(nes.steps());
        }
    } else if ui.input_mut(|i| i.consume_shortcut(&shortcuts.close)) {
        close();
    } else if ui.input_mut(|i| i.consume_shortcut(&shortcuts.reset)) {
        if let Some(steps) = &app.stepper {
            steps.reset();
        }
    }

    eframe::egui::menu::bar(ui, |ui| {
        ui.menu_button("File", |ui| {
            if ui
                .add(
                    Button::new("\u{1F5C1}  Open...")
                        .shortcut_text(ui.ctx().format_shortcut(&shortcuts.open_file)),
                )
                .clicked()
            {
                if let Some(path) = rfd::FileDialog::new().pick_file() {
                    let nes = yaner::nes::Nes::new(Cartridge::try_from(path).unwrap());
                    app.stepper = Some(nes.steps());
                }
                ui.close_menu();
            }

            if ui
                .add(
                    Button::new("\u{2386}  Exit")
                        .shortcut_text(ui.ctx().format_shortcut(&shortcuts.close)),
                )
                .clicked()
            {
                close();
            }
        });

        ui.menu_button("Emulation", |ui| {
            if ui
                .add_enabled(
                    app.stepper.is_some(),
                    Button::new("\u{267B}  Reset")
                        .shortcut_text(ui.ctx().format_shortcut(&shortcuts.reset)),
                )
                .clicked()
            {
                if let Some(steps) = &app.stepper {
                    steps.reset();
                    ui.close_menu();
                }
            }

            let mut close_menu = false;
            close_menu |= ui
                .radio_value(&mut app.settings.image_size_factor, 1, "1x")
                .clicked();
            close_menu |= ui
                .radio_value(&mut app.settings.image_size_factor, 2, "2x")
                .clicked();
            close_menu |= ui
                .radio_value(&mut app.settings.image_size_factor, 3, "3x")
                .clicked();

            if close_menu {
                ui.close_menu();
            }
        });
    })
}
