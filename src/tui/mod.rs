use std::io;

use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use tui::{Frame, Terminal};
use tui::backend::{Backend, TermionBackend};
use tui::layout::{Constraint, Direction, Layout, Rect};
use tui::style::{Modifier, Style};
use tui::text::{Span, Spans, Text};
use tui::widgets::{Block, Borders, Paragraph, Row, Table, Widget, ListItem, List, ListState};

use crate::memory::AddressSpace;
use crate::nes::{Nes, NesCycle, Stepper};
use crate::cpu::{Cpu, Flags};
use crate::cpu::opcode::OpCode;
use std::fmt::Display;
use bitflags::_core::fmt::Formatter;

impl Display for Flags {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for flag in vec![Flags::N, Flags::V, Flags::B, Flags::D, Flags::I, Flags::Z, Flags::C].into_iter() {
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

    let mut flags = nes.cpu.flags.get();

    let state = Text::from(vec![
        Spans::from(vec![
            Span::from(" PC: "),
            Span::styled(format!("{:04X}", nes.cpu.pc.get()), value_style)
        ]),
        Spans::from(vec![
            Span::from(" A: "),
            Span::styled(format!("{:02X}", nes.cpu.acc.get()), value_style)
        ]),
        Spans::from(vec![
            Span::from(" X: "),
            Span::styled(format!("{:02X}", nes.cpu.x.get()), value_style)
        ]),
        Spans::from(vec![
            Span::from(" Y: "),
            Span::styled(format!("{:02X}", nes.cpu.y.get()), value_style)
        ]),
        Spans::from(vec![
            Span::from(" P: "),
            Span::styled(format!("{:02X} {}", nes.cpu.flags(), flags), value_style)
        ]),
        Spans::from(vec![
            Span::from(" SP: "),
            Span::styled(format!("{:02X}", nes.cpu.sp.get()), value_style)
        ]),
        Spans::from(vec![
            Span::from(" CYC: "),
            Span::styled(format!("{}", nes.clocks.cpu_cycles.get()), value_style)
        ]),
    ]);
    Paragraph::new(state)
        .block(Block::default().title("CPU").borders(Borders::ALL))
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

        if addr == nes.cpu.pc.get() {
            selected = Some(items.len());
        }
        items.push(ListItem::new(instr));

        addr = addr.saturating_add(1).saturating_add(operand.1 as u16);
        if addr >= 0xFFFC {
            break;
        }
    };

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
        .constraints([Constraint::Length(9), Constraint::Percentage(100)].as_ref())
        .split(size);

    f.render_widget(cpu_block(nes), chunks[0]);
    prg_rom(f, &nes, chunks[1]);
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
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)/*, Constraint::Percentage(33)*/].as_ref())
        .split(size);

    f.render_widget(ram_block("RAM", &nes.cpu.bus.ram, 0, 0x800), chunks[0]);
    f.render_widget(ram_block("VRAM", &nes.cpu.bus.ppu_registers.bus.vram, 0x2000, 0x800), chunks[1]);
    // f.render_widget(ram_block("CHR-ROM", &nes.cpu.bus.ppu_registers.bus, 0, 0x2000), chunks[2]);
}

fn draw<B: Backend>(terminal: &mut Terminal<B>, nes: &Nes) -> Result<(), io::Error> {
    terminal.draw(|f| {
        let size = f.size();
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(90), Constraint::Min(10)].as_ref())
            .split(size);

        rams(f, &nes, chunks[0]);
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

    // powerup
    stepper.tick()?;
    loop {
        if running && !stepper.halted(){
            stepper.tick()?;
        }
        draw(&mut terminal, nes)?;
        if let Some(input_result) = input.next() {
            match input_result? {
                Key::Ctrl('c') | Key::Char('q') => break,
                Key::Char('f') => {
                    stepper.step_frame()?;
                },
                Key::Char('o') => {
                    stepper.step_cpu()?;
                },
                Key::Char('s') => {
                    stepper.tick()?;
                },
                Key::Char('r') => running = !running,
                _ => ()
            }
        }
    }

    Ok(())
}
