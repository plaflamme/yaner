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
use crate::memory::AddressSpace;
use crate::nes::debug::NesState;
use crate::nes::{Nes, Stepper};
use crate::ppu::{PpuCtrl, PpuStatus};

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
            PpuCtrl::N_LO,
            PpuCtrl::N_HI,
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
        for flag in vec![PpuStatus::O, PpuStatus::S, PpuStatus::V].into_iter() {
            if *self & flag == flag {
                write!(f, "{:?}", flag)?;
            } else {
                write!(f, "-")?;
            }
        }
        Ok(())
    }
}

fn cpu_block<'a>(state: &NesState<'a>) -> Paragraph<'a> {
    let value_style = Style::default().add_modifier(Modifier::BOLD);

    let intr_display = match state.cpu.intr {
        None => String::from("-"),
        Some(intr) => format!("{:?}", intr),
    };

    let state = Text::from(vec![
        Spans::from(vec![
            Span::from(" PC: "),
            Span::styled(format!("{:04X}", state.cpu.pc), value_style),
        ]),
        Spans::from(vec![
            Span::from(" A: "),
            Span::styled(format!("{:02X}", state.cpu.a), value_style),
        ]),
        Spans::from(vec![
            Span::from(" X: "),
            Span::styled(format!("{:02X}", state.cpu.x), value_style),
        ]),
        Spans::from(vec![
            Span::from(" Y: "),
            Span::styled(format!("{:02X}", state.cpu.y), value_style),
        ]),
        Spans::from(vec![
            Span::from(" P: "),
            Span::styled(
                format!("{:02X} {}", state.cpu.flags, state.cpu.flags),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from(" SP: "),
            Span::styled(format!("{:02X}", state.cpu.sp), value_style),
        ]),
        Spans::from(vec![
            Span::from(" INTR: "),
            Span::styled(format!("{}", intr_display), value_style),
        ]),
        Spans::from(vec![
            Span::from(" CYC: "),
            Span::styled(format!("{}", state.clocks.cpu_cycles), value_style),
        ]),
    ]);
    Paragraph::new(state).block(Block::default().title("CPU").borders(Borders::ALL))
}

fn ppu_block<'a>(nes: &NesState<'a>) -> Paragraph<'a> {
    let value_style = Style::default().add_modifier(Modifier::BOLD);

    let state = Text::from(vec![
        Spans::from(vec![
            Span::from(" C: "),
            Span::styled(
                format!("{:02X} {}", nes.ppu.ctrl, nes.ppu.ctrl),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from(" S: "),
            Span::styled(
                format!("{:02X} {}", nes.ppu.status, nes.ppu.status),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from(" T: "),
            Span::styled(format!("FY:{} NT:{}", nes.ppu.t_addr.fine_y(), nes.ppu.t_addr.nametable()), value_style),
        ]),
        Spans::from(vec![
            Span::from("    "),
            Span::styled(format!("CY:{} CX:{}", nes.ppu.t_addr.coarse_y(), nes.ppu.t_addr.coarse_x()), value_style),
        ]),
        Spans::from(vec![
            Span::from(" V: "),
            Span::styled(format!("FY:{} NT:{}", nes.ppu.v_addr.fine_y(), nes.ppu.v_addr.nametable()), value_style),
        ]),
        Spans::from(vec![
            Span::from("    "),
            Span::styled(format!("CY:{} CX:{}", nes.ppu.v_addr.coarse_y(), nes.ppu.v_addr.coarse_x()), value_style),
        ]),
        Spans::from(vec![
            Span::from(" CYC: "),
            Span::styled(format!("{},{}", nes.ppu.scanline, nes.ppu.dot), value_style),
        ]),
    ]);
    Paragraph::new(state).block(Block::default().title("PPU").borders(Borders::ALL))
}

// TODO: make sure the pc is in the middle of the list, or at least not at the bottom.
fn prg_rom<B: Backend>(
    f: &mut Frame<B>,
    state: &NesState,
    addr_space: &dyn AddressSpace,
    chunk: Rect,
) {
    let start = 0x8000;

    let mut items = Vec::new();
    let mut addr = start;
    let mut selected: Option<usize> = None;
    loop {
        let (OpCode(opcode, _), operand) = Cpu::decompile(addr, addr_space);
        let instr = Span::from(format!("{:04X} {:?} {}", addr, opcode, operand.0));

        // We can be in the middle of an instruction which has incremented pc,
        //   use the closest instruction to the current pc value if there's no exact match.
        if addr <= state.cpu.pc {
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

fn rightbar<B: Backend>(
    f: &mut Frame<B>,
    nes: &NesState,
    addr_space: &dyn AddressSpace,
    size: Rect,
) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints(
            [
                Constraint::Length(10),
                Constraint::Length(9),
                Constraint::Percentage(100),
            ]
            .as_ref(),
        )
        .split(size);

    f.render_widget(cpu_block(nes), chunks[0]);
    f.render_widget(ppu_block(nes), chunks[1]);
    prg_rom(f, &nes, addr_space, chunks[2]);
}

struct MemoryBlock<'a> {
    name: &'a str,
    addr_space: &'a dyn AddressSpace,
    base: u16,
    size: u16,
}

impl<'a> MemoryBlock<'a> {
    fn new(name: &'a str, addr_space: &'a dyn AddressSpace, base: u16, size: u16) -> Self {
        MemoryBlock {
            name,
            addr_space,
            base,
            size,
        }
    }

    fn to_block(&self, shift: u16) -> impl Widget + '_ {
        let rows = (self.base + (shift * 16)..self.base.saturating_add(self.size))
            .step_by(16)
            .map(move |base| {
                std::iter::once(format!("${:04X}:", base)).chain(
                    (0..16)
                        .map(move |low| self.addr_space.read_u8(base + low))
                        .map(|v| format!("{:02X}", v)),
                )
            })
            .map(|row| Row::Data(row));

        Table::new(std::iter::empty::<&str>(), rows)
            .block(Block::default().title(self.name).borders(Borders::ALL))
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
}

fn h_rams<'a, B: Backend>(
    f: &mut Frame<B>,
    left: &MemoryBlock<'a>,
    right: &MemoryBlock<'a>,
    shift: u16,
    size: Rect,
) {
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(vec![Constraint::Length(55), Constraint::Length(55)])
        .split(size);
    f.render_widget(left.to_block(shift), chunks[0]);
    f.render_widget(right.to_block(shift), chunks[1]);
}

fn rams<'a, B: Backend>(f: &mut Frame<B>, nes: &NesState<'a>, shift: u16, size: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(size);

    h_rams(
        f,
        &MemoryBlock::new("RAM", nes.ram, 0, 0x0800),
        &MemoryBlock::new("VRAM", nes.vram, 0x2000, 0x0800),
        shift,
        chunks[0],
    );
    h_rams(
        f,
        &MemoryBlock::new("PRG-ROM", nes.prg_rom, 0x8000, 0x8000),
        &MemoryBlock::new("CHR-ROM", nes.chr_rom, 0x0, 0x2000),
        shift,
        chunks[1],
    );
}

fn draw<'a, B: Backend>(
    terminal: &mut Terminal<B>,
    state: &NesState<'a>,
    shift: u16,
) -> Result<(), io::Error> {
    terminal.draw(|f| {
        let size = f.size();
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Length(110), Constraint::Min(15)].as_ref())
            .split(size);

        rams(f, &state, shift, chunks[0]);
        rightbar(f, &state, state.prg_rom, chunks[1]);
    })
}

// TODO: implement debugger state here.
pub struct Debugger {
    nes: Nes,
}

impl Debugger {
    pub fn new(nes: Nes) -> Self {
        Debugger {
            nes,
        }
    }

    pub fn start(&self, start_at: Option<u16>) -> Result<(), anyhow::Error> {
        let stdout = io::stdout().into_raw_mode()?;
        let backend = TermionBackend::new(stdout);
        let mut terminal = Terminal::new(backend)?;
        terminal.clear()?;

        let mut stepper = Stepper::new(&self.nes, start_at);
        let mut input = io::stdin().keys();

        let mut shift = 0u16;

        loop {
            if stepper.halted() {
                break;
            }
            draw(&mut terminal, &self.nes.debug(), shift)?;
            if let Some(input_result) = input.next() {
                match input_result? {
                    Key::Ctrl('c') | Key::Char('q') => break,
                    Key::Char('f') => {
                        stepper.step_frame()?;
                    }
                    Key::Char('o') => {
                        stepper.step_cpu()?;
                    }
                    Key::Char(' ') | Key::Char('s') => {
                        stepper.tick()?;
                    }
                    Key::Char('v') => {
                        stepper.tick_until(|nes| nes.debug().ppu.status.contains(PpuStatus::V))?;
                    }
                    Key::Char('n') => {
                        stepper.tick_until(|nes| match nes.debug().cpu.intr {
                            Some(Interrupt::Nmi) => true,
                            _ => false,
                        })?;
                    }
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
}
