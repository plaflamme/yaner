use std::io;

use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use tui::{Frame, Terminal};
use tui::backend::{Backend, TermionBackend};
use tui::layout::{Constraint, Direction, Layout, Rect};
use tui::style::{Modifier, Style};
use tui::text::{Span, Spans};
use tui::widgets::{Block, Borders, Paragraph, Row, Table, Widget};

use crate::cpu::CpuCycle;
use crate::memory::AddressSpace;
use crate::nes::{Nes, NesCycle};
use crate::ppu::PpuCycle;

fn cpu_block(nes: &Nes) -> Paragraph {

    let value_style = Style::default().add_modifier(Modifier::BOLD);

    let state = Spans::from(vec![
        Span::from(" PC: "),
        Span::styled(format!("{:04X}", nes.cpu.pc.get()), value_style),
        Span::from(" A: "),
        Span::styled(format!("{:02X}", nes.cpu.acc.get()), value_style),
        Span::from(" X: "),
        Span::styled(format!("{:02X}", nes.cpu.x.get()), value_style),
        Span::from(" Y: "),
        Span::styled(format!("{:02X}", nes.cpu.y.get()), value_style),
        Span::from(" P: "),
        Span::styled(format!("{:02X}", nes.cpu.flags()), value_style),
        Span::from(" SP: "),
        Span::styled(format!("{:02X}", nes.cpu.sp.get()), value_style),
    ]);
    Paragraph::new(state)
        .block(Block::default().title("CPU").borders(Borders::ALL))
}

fn ram_block<'a>(name: &'a str, addr_space: &'a dyn AddressSpace, base: u16, size: u16) -> impl Widget + 'a {

    let header = (0..16).map(|_| "");

    let rows = (base..(base + size))
        .step_by(16)
        .map(move |base| {
            (0..16)
                .map(move |low| addr_space.read_u8(base + low))
                .map(|v| format!("{:02X}", v))
        })
        .map(|row| Row::Data(row));

    Table::new(header, rows)
        .block(Block::default().title(name).borders(Borders::ALL))
        .widths(&[Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3), Constraint::Length(3)])
        .header_gap(0)
}

fn rams<'a, B: Backend>(f: &mut Frame<B>, nes: &'a Nes, size: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(33), Constraint::Percentage(33), Constraint::Percentage(33)].as_ref())
        .split(size);

    f.render_widget(ram_block("RAM", &nes.cpu.bus.ram, 0, 0x800), chunks[0]);
    f.render_widget(ram_block("VRAM", &nes.cpu.bus.ppu_registers.bus.vram, 0x2000, 0x800), chunks[1]);
    f.render_widget(ram_block("CHR-ROM", &nes.cpu.bus.ppu_registers.bus, 0, 0x2000), chunks[2]);
}

fn draw<B: Backend>(terminal: &mut Terminal<B>, nes: &Nes) -> Result<(), io::Error> {
    terminal.draw(|f| {
        let size = f.size();
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Length(3), Constraint::Percentage(100)].as_ref())
            .split(size);

        f.render_widget(cpu_block(nes), chunks[0]);
        rams(f, &nes, chunks[1]);
    })
}

pub fn main(nes: &Nes, start_at: Option<u16>) -> Result<(), io::Error> {
    let stdout = io::stdout().into_raw_mode()?;
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let mut stepper = nes.ppu_stepper(start_at);
    let mut input = termion::async_stdin().keys();

    let mut running = false;

    // powerup
    stepper.step();
    loop {
        if running {
            stepper.step();
        }
        draw(&mut terminal, nes)?;
        if let Some(input_result) = input.next() {
            match input_result? {
                Key::Ctrl('c') | Key::Char('q') => break,
                Key::Char('f') => loop {
                    match stepper.step() {
                        NesCycle::CpuCycle(_, PpuCycle::Frame) => break,
                        NesCycle::PpuCycle(PpuCycle::Frame) => break,
                        _ => ()
                    }
                },
                Key::Char('o') => loop {
                    match stepper.step() {
                        NesCycle::CpuCycle(CpuCycle::OpComplete(_, _), _) => break,
                        _ => ()
                    }
                },
                Key::Char('s') => {
                    stepper.step();
                },
                Key::Char('r') => running = !running,
                _ => ()
            }
        }
    }

    Ok(())
}
