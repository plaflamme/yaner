use std::fmt::{Display, Formatter};
use std::io;

use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use tui::backend::{Backend, TermionBackend};
use tui::layout::{Constraint, Direction, Layout, Rect};
use tui::style::{Modifier, Style};
use tui::text::{Span, Spans, Text};
use tui::widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Row, Table, Widget};
use tui::{Frame, Terminal};

use crate::cpu::opcode::OpCode;
use crate::cpu::{Cpu, Flags, Interrupt};
use crate::ppu::{PpuStatus, PpuCtrl};
use crate::memory::AddressSpace;
use crate::nes::{Nes, Stepper};

impl Display for Flags {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for flag in vec![
            Flags::N,
            Flags::V,
            Flags::B,
            Flags::D,
            Flags::I,
            Flags::Z,
            Flags::C,
        ]
        .into_iter()
        {
            if *self & flag == flag {
                write!(f, "{:?}", flag)?;
            } else {
                write!(f, "-")?;
            }
        }
        Ok(())
    }
}

impl Display for PpuCtrl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for flag in vec![
            // PpuCtrl::N_LO,
            // PpuCtrl::N_HI,
            PpuCtrl::I,
            PpuCtrl::S,
            PpuCtrl::B,
            PpuCtrl::H,
            PpuCtrl::P,
            PpuCtrl::V,
        ]
            .into_iter()
        {
            if *self & flag == flag {
                write!(f, "{:?}", flag)?;
            } else {
                write!(f, "-")?;
            }
        }
        Ok(())
    }
}

impl Display for PpuStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for flag in vec![
            PpuStatus::O,
            PpuStatus::S,
            PpuStatus::V,
        ]
            .into_iter()
        {
            if *self & flag == flag {
                write!(f, "{:?}", flag)?;
            } else {
                write!(f, "-")?;
            }
        }
        Ok(())
    }
}

fn cpu_block(nes: &Nes) -> Paragraph {
    let value_style = Style::default().add_modifier(Modifier::BOLD);

    let state = Text::from(vec![
        Spans::from(vec![
            Span::from(" PC: "),
            Span::styled(format!("{:04X}", nes.cpu.pc.get()), value_style),
        ]),
        Spans::from(vec![
            Span::from(" A: "),
            Span::styled(format!("{:02X}", nes.cpu.acc.get()), value_style),
        ]),
        Spans::from(vec![
            Span::from(" X: "),
            Span::styled(format!("{:02X}", nes.cpu.x.get()), value_style),
        ]),
        Spans::from(vec![
            Span::from(" Y: "),
            Span::styled(format!("{:02X}", nes.cpu.y.get()), value_style),
        ]),
        Spans::from(vec![
            Span::from(" P: "),
            Span::styled(
                format!("{:02X} {}", nes.cpu.flags(), nes.cpu.flags.get()),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from(" SP: "),
            Span::styled(format!("{:02X}", nes.cpu.sp.get()), value_style),
        ]),
        Spans::from(vec![
            Span::from(" INTR: "),
            Span::styled(format!("{:?}", nes.cpu.bus.intr.get()), value_style),
        ]),
        Spans::from(vec![
            Span::from(" CYC: "),
            Span::styled(format!("{}", nes.clocks.cpu_cycles.get()), value_style),
        ]),
    ]);
    Paragraph::new(state).block(Block::default().title("CPU").borders(Borders::ALL))
}

fn ppu_block(nes: &Nes) -> Paragraph {
    let value_style = Style::default().add_modifier(Modifier::BOLD);

    let state = Text::from(vec![
        Spans::from(vec![
            Span::from(" C: "),
            Span::styled(
                format!("{:02X} {}", nes.ppu.ctrl.get(), nes.ppu.ctrl.get()),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from(" S: "),
            Span::styled(
                format!("{:02X} {}", nes.ppu.status.get(), nes.ppu.status.get()),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from(" OAM: "),
            Span::styled(format!("{:04X}", nes.ppu.oam_addr.get()), value_style),
        ]),
        Spans::from(vec![
            Span::from(" CYC: "),
            Span::styled(format!("{},{}", nes.ppu.scanline.get(), nes.ppu.dot.get()), value_style),
        ]),
    ]);
    Paragraph::new(state).block(Block::default().title("PPU").borders(Borders::ALL))
}

// TODO: make sure the pc is in the middle of the list, or at least not at the bottom.
fn prg_rom<B: Backend>(f: &mut Frame<B>, nes: &Nes, chunk: Rect) {
    let mapper = nes.cpu.bus.mapper.borrow();
    let addr_space = mapper.cpu_addr_space();

    let start = 0x8000;

    let mut items = Vec::new();
    let mut addr = start;
    let mut selected: Option<usize> = None;
    loop {
        let (OpCode(opcode, _), operand) = Cpu::decompile(addr, addr_space);
        let instr = Span::from(format!("{:04X} {:?} {}", addr, opcode, operand.0));

        // We can be in the middle of an instruction which has incremented pc,
        //   use the closest instruction to the current pc value if there's no exact match.
        if addr <= nes.cpu.pc.get() {
            selected = Some(items.len());
        }
        items.push(ListItem::new(instr));

        addr = addr.saturating_add(1).saturating_add(operand.1 as u16);
        if addr >= 0xFFFE {
            break;
        }
    }

    let mut state = ListState::default();
    state.select(selected);
    let list = List::new(items)
        .block(Block::default().title("PRG").borders(Borders::ALL))
        .highlight_style(Style::default().add_modifier(Modifier::BOLD))
        .highlight_symbol(">");

    f.render_stateful_widget(list, chunk, &mut state);
}

fn rightbar<B: Backend>(f: &mut Frame<B>, nes: &Nes, size: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Length(10), Constraint::Length(6), Constraint::Percentage(100)].as_ref())
        .split(size);

    f.render_widget(cpu_block(nes), chunks[0]);
    f.render_widget(ppu_block(nes), chunks[1]);
    prg_rom(f, &nes, chunks[2]);
}

fn ram_block<'a>(
    name: &'a str,
    addr_space: &'a dyn AddressSpace,
    base: u16,
    size: u16,
    shift: u16,
) -> impl Widget + 'a {
    let rows = (base + (shift * 16)..base.saturating_add(size))
        .step_by(16)
        .map(move |base| {
            std::iter::once(format!("${:04X}:", base)).chain(
                (0..16)
                    .map(move |low| addr_space.read_u8(base + low))
                    .map(|v| format!("{:02X}", v)),
            )
        })
        .map(|row| Row::Data(row));

    Table::new(std::iter::empty::<&str>(), rows)
        .block(Block::default().title(name).borders(Borders::ALL))
        .widths(&[
            Constraint::Length(7),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
            Constraint::Length(2),
        ])
        .header_gap(0)
}

fn rams<'a, B: Backend>(f: &mut Frame<B>, nes: &'a Nes, shift: u16, size: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(
            [
                Constraint::Length(55),
                Constraint::Length(55),
                Constraint::Length(55),
            ]
            .as_ref(),
        )
        .split(size);

    f.render_widget(ram_block("RAM", &nes.cpu.bus.ram, 0, 0x800, shift), chunks[0]);
    f.render_widget(
        ram_block("VRAM", &nes.cpu.bus.ppu_registers.bus.vram, 0x2000, 0x800, shift),
        chunks[1],
    );
    f.render_widget(
        ram_block("PRG-ROM", &nes.cpu.bus, 0x8000, 0x8000, shift),
        chunks[2],
    );
    // f.render_widget(
    //     ram_block("CHR-ROM", &nes.cpu.bus.ppu_registers.bus, 0, 0x2000, shift),
    //     chunks[2],
    // );
}

fn draw<B: Backend>(terminal: &mut Terminal<B>, nes: &Nes, shift: u16) -> Result<(), io::Error> {
    terminal.draw(|f| {
        let size = f.size();
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(90), Constraint::Min(10)].as_ref())
            .split(size);

        rams(f, &nes, shift, chunks[0]);
        rightbar(f, &nes, chunks[1]);
    })
}

pub fn main(nes: &Nes, start_at: Option<u16>) -> Result<(), anyhow::Error> {
    let stdout = io::stdout().into_raw_mode()?;
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let mut stepper = Stepper::new(nes, start_at);
    let mut input = termion::async_stdin().keys();

    let mut running = false;
    let mut shift = 0u16;

    // powerup
    stepper.tick()?;
    loop {
        if running && !stepper.halted() {
            stepper.tick()?;
        }
        draw(&mut terminal, nes, shift)?;
        if let Some(input_result) = input.next() {
            match input_result? {
                Key::Ctrl('c') | Key::Char('q') => break,
                Key::Char('f') => {
                    stepper.step_frame()?;
                }
                Key::Char('o') => {
                    stepper.step_cpu()?;
                }
                Key::Char('s') => {
                    stepper.tick()?;
                }
                Key::Char('v') => {
                    stepper.tick_until(|nes|{
                        nes.ppu.status.get().contains(PpuStatus::V)
                    })?;
                }
                Key::Char('n') => {
                    stepper.tick_until(|nes|{
                        match nes.cpu.bus.intr.get() {
                            Some(Interrupt::Nmi) => true,
                            _ => false
                        }
                    })?;
                }
                Key::Char('r') => running = !running,
                Key::Down => shift = shift.saturating_add(1),
                Key::PageDown => shift = shift.saturating_add(16),
                Key::PageUp => shift = shift.saturating_sub(16),
                Key::Up => shift = shift.saturating_sub(1),
                _ => (),
            }
        }
    }

    Ok(())
}
