use std::fmt::{Display, Formatter};
use std::io;

use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use tui::backend::{Backend, TermionBackend};
use tui::layout::{Constraint, Direction, Layout, Rect};
use tui::style::{Color, Modifier, Style};
use tui::text::{Span, Spans, Text};
use tui::widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Row, Table, Widget};
use tui::{Frame, Terminal};

use crate::cpu::opcode::OpCode;
use crate::cpu::{Cpu, Flags, Interrupt};
use crate::memory::AddressSpace;
use crate::nes::debug::NesState;
use crate::nes::{Nes, Stepper};
use crate::ppu::reg::{PpuCtrl, PpuMask, PpuStatus};

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

impl Display for PpuMask {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for flag in vec![
            PpuMask::M,
            PpuMask::m,
            PpuMask::b,
            PpuMask::s,
            PpuMask::R,
            PpuMask::G,
            PpuMask::B,
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
            Span::from(" M: "),
            Span::styled(
                format!("{:02X} {}", nes.ppu.mask, nes.ppu.mask),
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
            Span::styled(
                format!(
                    "FY:{} NT:{}",
                    nes.ppu.t_addr.fine_y(),
                    nes.ppu.t_addr.nametable()
                ),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from("    "),
            Span::styled(
                format!(
                    "CY:{} CX:{}",
                    nes.ppu.t_addr.coarse_y(),
                    nes.ppu.t_addr.coarse_x()
                ),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from(" V: "),
            Span::styled(
                format!(
                    "FY:{} NT:{}",
                    nes.ppu.v_addr.fine_y(),
                    nes.ppu.v_addr.nametable()
                ),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from("    "),
            Span::styled(
                format!(
                    "CY:{} CX:{}",
                    nes.ppu.v_addr.coarse_y(),
                    nes.ppu.v_addr.coarse_x()
                ),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from(" FX: "),
            Span::styled(format!("{}", nes.ppu.fine_x), value_style),
        ]),
        Spans::from(vec![
            Span::from(" P: "),
            Span::styled(
                format!(
                    "{:02X} {:04X} {:02X} {:04X}",
                    nes.ppu.pattern_data.latch.high.get(),
                    nes.ppu.pattern_data.value.high.get(),
                    nes.ppu.pattern_data.latch.low.get(),
                    nes.ppu.pattern_data.value.low.get()
                ),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from(" A: "),
            Span::styled(
                format!(
                    "{:02X} {:02X} {:02X} {:02X}",
                    nes.ppu.attribute_data.latch.high.get(),
                    nes.ppu.attribute_data.value.high.get(),
                    nes.ppu.attribute_data.latch.low.get(),
                    nes.ppu.attribute_data.value.low.get()
                ),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from(" O: "),
            Span::styled(
                format!("{:02X}",nes.ppu.oam_addr,),
                value_style,
            ),
        ]),
        Spans::from(vec![
            Span::from(" CYC: "),
            Span::styled(
                format!(
                    "{}@{},{}",
                    nes.clocks.ppu_frames, nes.ppu.scanline, nes.ppu.dot
                ),
                value_style,
            ),
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

fn sprite_block<'a>(nes: &NesState<'a>) -> Paragraph<'a> {
    let value_style = Style::default().add_modifier(Modifier::BOLD);

    let mut s_spans = Vec::new();

    s_spans.push(
        Spans::from(vec![
            Span::from(" OE: "),
            Span::styled(
                format!("{:02X}",nes.ppu.sprite_pipeline.oam_entry),
                value_style,
            ),
        ])
    );
    let s_oam = nes.ppu.sprite_pipeline.secondary_oam;

    s_spans.push(Spans::from(Span::from(" SOAM: ")));
    for i in 0..8 as usize {
        let base = i * 4;
        let span = Span::styled(
            format!("    {:02X} {:02X} {:02X} {:02X}", s_oam[base], s_oam[base+1], s_oam[base+2], s_oam[base+3]),
            value_style,
        );
        s_spans.push(Spans::from(span));
    }

    s_spans.push(Spans::from(Span::from(" OUT: ")));
    for output in nes.ppu.sprite_pipeline.output_units.iter() {
        let span = match output {
            None => "    -".to_owned(),
            Some(sprite) =>
                format!("    {},{} {:02X}", sprite.sprite.y, sprite.sprite.x, sprite.sprite.attr.raw())
        };
        s_spans.push(Spans::from(Span::styled(span, value_style)));
    }

    Paragraph::new(Text::from(s_spans)).block(Block::default().title("OAM").borders(Borders::ALL))
}

fn statusbar<B: Backend>(
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
                Constraint::Length(14),
                Constraint::Length(21),
                Constraint::Percentage(100),
            ]
            .as_ref(),
        )
        .split(size);

    f.render_widget(cpu_block(nes), chunks[0]);
    f.render_widget(ppu_block(nes), chunks[1]);
    f.render_widget(sprite_block(nes), chunks[2]);
    prg_rom(f, &nes, addr_space, chunks[3]);
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

fn frame<'a, B: Backend>(f: &mut Frame<B>, nes: &NesState<'a>, shift: Shift, size: Rect) {
    // read the value of the bg color
    let mut frame = Vec::new();
    for sl in shift.down..240 {
        let mut line = Vec::new();
        for dot in shift.right..256 {
            let pixel = nes.ppu.frame[(sl * 256 + dot) as usize];
            let (r, g, b) = pixel.rgb();
            let style = Style::default().fg(Color::Rgb(r, g, b));
            line.push(Span::styled(String::from(tui::symbols::block::FULL), style));
        }
        frame.push(Spans::from(line));
    }
    let widget = Paragraph::new(Text::from(frame))
        .block(Block::default().title("Frame").borders(Borders::ALL));

    f.render_widget(widget, size);
}

fn draw<'a, B: Backend>(
    terminal: &mut Terminal<B>,
    state: &NesState<'a>,
    app_state: AppState,
) -> Result<(), io::Error> {
    terminal.draw(|f| {
        let size = f.size();
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Length(21), Constraint::Percentage(100)].as_ref())
            .split(size);

        statusbar(f, &state, state.prg_rom, chunks[0]);
        match app_state.main_view {
            View::Memory => rams(f, &state, app_state.shift.down, chunks[1]),
            View::Frame => frame(f, &state, app_state.shift, chunks[1]),
        };
    })
}

#[derive(Clone, Copy, Default)]
struct Shift {
    down: u16,
    right: u16,
}

impl Shift {
    fn down(&mut self) {
        self.down_by(1);
    }
    fn down_by(&mut self, size: u16) {
        self.down = self.down.saturating_add(size);
    }
    fn up(&mut self) {
        self.up_by(1);
    }
    fn up_by(&mut self, size: u16) {
        self.down = self.down.saturating_sub(size);
    }
    fn right(&mut self) {
        self.right_by(1);
    }
    fn right_by(&mut self, by: u16) {
        self.right = self.right.saturating_add(by);
    }
    fn left(&mut self) {
        self.left_by(1);
    }
    fn left_by(&mut self, by: u16) {
        self.right = self.right.saturating_sub(by);
    }
}

#[derive(Clone, Copy)]
enum View {
    Memory,
    Frame,
}

#[derive(Clone, Copy)]
struct AppState {
    main_view: View,
    shift: Shift,
}

impl AppState {
    fn new() -> Self {
        AppState {
            main_view: View::Memory,
            shift: Shift::default(),
        }
    }

    fn cycle_view(&mut self) {
        match self.main_view {
            View::Memory => self.main_view = View::Frame,
            View::Frame => self.main_view = View::Memory,
        }
    }
}

// TODO: implement debugger state here.
pub struct Debugger {
    nes: Nes,
}

impl Debugger {
    pub fn new(nes: Nes) -> Self {
        Debugger { nes }
    }

    pub fn start(&self, start_at: Option<u16>) -> Result<(), anyhow::Error> {
        let stdout = io::stdout().into_raw_mode()?;
        let backend = TermionBackend::new(stdout);
        let mut terminal = Terminal::new(backend)?;
        terminal.clear()?;

        let mut stepper = Stepper::new(&self.nes, start_at);
        let mut input = io::stdin().keys();

        let mut app_state = AppState::new();

        loop {
            if stepper.halted() {
                break;
            }
            draw(&mut terminal, &self.nes.debug(), app_state)?;
            if let Some(input_result) = input.next() {
                match input_result? {
                    Key::Char('[') | Key::Char(']') => app_state.cycle_view(),
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
                    Key::Char('l') => {
                        let current_sl = self.nes.debug().ppu.scanline;
                        stepper.tick_until(|nes| nes.debug().ppu.scanline != current_sl)?;
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
                    Key::Down => app_state.shift.down(),
                    Key::PageDown => app_state.shift.down_by(16),
                    Key::Up => app_state.shift.up(),
                    Key::PageUp => app_state.shift.up_by(16),
                    Key::Right => app_state.shift.right(),
                    Key::Left => app_state.shift.left(),
                    _ => (),
                }
            }
        }

        Ok(())
    }
}
