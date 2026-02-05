#![feature(assert_matches)]
#![allow(non_upper_case_globals, non_snake_case)]

#[allow(unused)]
mod common;

use yaner::nes::debug::Instruction;

use crate::common::run_test_with_steps;
use std::sync::Once;
use std::sync::mpsc::RecvTimeoutError;
use std::{assert_matches, collections::HashMap, path::PathBuf};
use test_case::test_case;

#[allow(dead_code)]
#[derive(Debug)]
enum Error {
    Code(u8),
    Timeout,
    Panic,
    Other(String),
}

impl From<anyhow::Error> for Error {
    fn from(value: anyhow::Error) -> Self {
        Self::Other(value.to_string())
    }
}

static INIT: Once = Once::new();

fn setup_logging() {
    INIT.call_once(|| {
        // Initialize the logger only once
        // set is_test(true) to ensure logs are captured by the test harness
        let _ = env_logger::builder().is_test(true).try_init();
    });
}

struct AccuracyCoin {
    root: PathBuf,
    listing: HashMap<String, u16>,
    labels: HashMap<u16, String>,
}

impl AccuracyCoin {
    fn new(root: impl Into<PathBuf>) -> anyhow::Result<Self> {
        let root = root.into();
        let fns = root.join("AccuracyCoin.fns");
        let fns = std::fs::read_to_string(fns)?;
        let listing = fns
            .lines()
            .map(|l| l.trim())
            .filter(|l| !l.starts_with(";"))
            .map(|l| {
                let parse = || -> anyhow::Result<(String, u16)> {
                    let (fns, addr) = l
                        .split_once("=")
                        .ok_or(anyhow::format_err!("Invalid line {l}"))?;
                    let addr = u16::from_str_radix(addr.trim().trim_start_matches('$'), 16)?;
                    Ok((fns.trim().to_string(), addr))
                };
                parse()
            })
            .collect::<anyhow::Result<HashMap<String, u16>>>()?;
        let labels = listing
            .iter()
            .map(|(k, &v)| (v, k.clone()))
            .collect::<HashMap<u16, String>>();

        Ok(Self {
            root,
            listing,
            labels,
        })
    }

    fn addr(&self, label: &str) -> anyhow::Result<u16> {
        self.listing
            .get(label)
            .copied()
            .ok_or_else(|| anyhow::format_err!("Cannot find address of label {label}"))
    }

    fn label(&self, addr: u16) -> Option<&str> {
        self.labels.get(&addr).map(|s| s.as_str())
    }

    fn rom(&self) -> PathBuf {
        self.root.join("AccuracyCoin.nes")
    }
}

// What's going on here.
//
// AccuracyCoin uses menu-driven test selection. You can run individual tests (press A), whole suites (press A at the top)
// or all of the suites (press start). You can also mark some tests to skip (press B). You can change suites by pressing left/right
// when highlighting the top of a page.
//
// Furthermore, the suites and individual tests are organized in the .asm file as data tables.
// When a test needs to run, the program will write a JSR instruction to that test in RAM at runtime.
// Every test is organized to write its result (code) in A which is the written to a pre-determined location in RAM.
// Each test has its own location, allowing to display a summary table of all test results.
//
// Since tests run from a UI, this happens within NMI. That is, the startup NMI routine will check the joypad, the state
// of the menu, i.e.: which page (X coordinate) and which test (Y coordinate) is highlighted and then it will setup
// the pointers in RAM for the JSR to occur to that test's code.
//
// This test harness hijacks the NMI routine to execute specific tests:
// * first we wait for the PC to point at NMI_Routine
// * then we jump over the ReadController1 function which writes the controller button to $controller_New
// * at this point, we write the test we want to run in the following locations in RAM:
//   * write the suite's PC to $suitePointer
//   * write 1 in $menuCursorYPos (as if we're selecting the first test of the suite)
//   * write the test's PC to $suiteExecPointerList+2 (the +2 is because we're pointing at the 1st test in the suite, the 0th is the suite itself)
//   * write the address where we want the result to be stored to $suitePointerList+2
//   * wriate 0x80 to $controller_New, mimics pressing A on the joypad
// * we let the NMI_Routine continue until RunTest which we jump over
// * ==> test runs
// * on RTS, we look at the result in the RAM location specified earlier.
//   * if the value is 1, this returns Ok(())
//   * otherwise, this returns Err(Error::Code(result >> 2)), see below
//
// NOTE: In order to limit the number of hardcoded constants, we extract the label address we care about
// out of the listing file (AccuracyCoin.fns) which is parsed at the beginnig of each test.
fn run_accuracy_coin_test(suite: &str, test: &str) -> anyhow::Result<(), Error> {
    setup_logging();
    let accuracy_coin = AccuracyCoin::new("roms/AccuracyCoin")?;
    let suite_pointer = accuracy_coin.addr(suite)?;
    let test_exec_pointer = accuracy_coin.addr(test)?;

    let NMI_Routine = accuracy_coin.addr("NMI_Routine")?;
    let ReadController1 = accuracy_coin.addr("ReadController1")?;
    let RunTest = accuracy_coin.addr("RunTest")?;

    // TODO: read these constants out of .asm
    const controller_New: u16 = 0x19;
    const suitePointer: u16 = 0x05;
    // Address of pointer to test to run
    const suiteExecPointerList: u16 = 0xA0;
    // Address of pointer where to store test result
    const suitePointerList: u16 = 0x80;
    const menuCursorYPos: u16 = 0x16;

    // TODO: should we change this to align where the tests actually should write their result?
    const ResultAddr: u16 = 0x0403;

    enum State {
        Setup,
        Running,
    }
    let mut state = State::Setup;
    let mut breakpoint: Option<u16> = None;
    let mut last_label = "N/A".to_string();
    run_test_with_steps(
        accuracy_coin.rom(),
        None,
        |steps| steps.step_cpu().map(|_| ()),
        |nes_state| {
            if let Some(label) = accuracy_coin.label(nes_state.cpu.pc)
                && label != last_label
            {
                log::debug!("Reached label (0x{:02X}): {}", nes_state.cpu.pc, label);
                // TODO: count instead of skipping
                last_label = label.to_string();
            }
            match state {
                State::Setup => {
                    if nes_state.cpu.active_pc == NMI_Routine {
                        // Check our assumption before setting the breakpoint
                        assert_matches!(
                            nes_state.active_op(),
                            Instruction { op: yaner_cpu::Op::JSR, mode: _, operand } if operand == ReadController1
                        );
                        breakpoint = Some(nes_state.cpu.active_pc + 3);
                    } else if let Some(bp) = breakpoint
                        && nes_state.cpu.active_pc == bp
                    {
                        breakpoint = None;
                        state = State::Running;

                        // Make sure this is 1:
                        //   * if the value is 0, it indicates that the cursor is at the top and will not allow pressing A to run a test
                        //   * since this is used for indexing info suitePointer*Lists, we have to adjust where we write the test to run below
                        nes_state.ram.write_u8(menuCursorYPos, 1);
                        // Suite to run
                        nes_state.ram.write_u16(suitePointer, suite_pointer);
                        // Test within suite to run
                        nes_state
                            .ram
                            .write_u16(suiteExecPointerList + 2, test_exec_pointer);
                        // Where to write the test result
                        nes_state.ram.write_u16(suitePointerList + 2, ResultAddr);

                        // Press A - runs the test
                        nes_state.ram.write_u8(controller_New, 0x80);
                    }
                }
                State::Running => {
                    if let Instruction {
                        op: yaner_cpu::Op::JSR,
                        mode: _,
                        operand,
                    } = nes_state.active_op()
                        && operand == RunTest
                    {
                        breakpoint = Some(nes_state.cpu.active_pc + 3);
                    } else if let Some(bp) = breakpoint
                        && nes_state.cpu.active_pc == bp
                    {
                        return common::Eval::Halt;
                    }
                }
            }

            common::Eval::Continue
        },
        |nes_state| {
            /*
            ; 0 = "TEST"
            ; 1 = "PASS"
            ; 2 = "FAIL"
            ; 3 = "...." for a test in progress.
            ; 4 = "DRAW"
            ; 5 = "SKIP"
            ; bits 0 and 1 hold the results. Bits 3+ hold error codes for printing what failed.
            */
            // https://github.com/100thCoin/AccuracyCoin/pull/39
            let result_addr = if test == "TEST_PowerOnState_PPU_ResetFlag" {
                0x0360
            } else {
                ResultAddr
            };
            let result = nes_state.ram.read_u8(result_addr);
            if result == 1 {
                Ok(())
            } else {
                log::debug!("{result}");
                Err(Error::Code(result >> 2))
            }
        },
    )
}

fn run_accuracy_coin_test_with_timeout(
    suite: &'static str,
    test: &'static str,
) -> anyhow::Result<(), Error> {
    let (sender, receiver) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let result = match std::panic::catch_unwind(|| run_accuracy_coin_test(suite, test)) {
            Ok(result) => result,
            Err(_) => Err(Error::Panic),
        };
        sender.send(result).unwrap();
    });
    match receiver.recv_timeout(std::time::Duration::from_secs(15)) {
        Ok(result) => result,
        Err(RecvTimeoutError::Timeout) => Err(Error::Timeout),
        Err(RecvTimeoutError::Disconnected) => panic!("unexpected panic in test thread"),
    }
}

macro_rules! test_suite {
    ($suite_name: ident, $($test_name:ident $(=> $matches:pat)?),+) => {

        $(
            const $test_name: &str = stringify!($test_name);
        )+

        $(#[test_case($test_name $( => matches $matches )? )])+
        fn $suite_name(test: &'static str) -> anyhow::Result<(), Error> {
            run_accuracy_coin_test_with_timeout(stringify!($suite_name), test)
        }
    };
}

test_suite! {
    Suite_CPUBehavior,
    TEST_ROMnotWritable,
    TEST_RamMirroring,
    Test_ProgramCounter_Wraparound,
    TEST_DecimalFlag,
    TEST_BFlag => Err(Error::Code(3)), // requires APU
    TEST_DummyReads,
    TEST_DummyWrites,
    TEST_OpenBus => Err(Error::Code(1)), // Reading from open bus is not all zeroes.
    TEST_AllNOPs => Err(Error::Code(2)) // Opcode $0C (NOP Absolute) malfunctioned. - but I think this is dummy read
}

test_suite! {
    Suite_CPUInstructions,
    TEST_AddrMode_AbsIndex,
    TEST_AddrMode_ZPgIndex,
    TEST_AddrMode_Indirect,
    TEST_AddrMode_IndIndeX,
    TEST_AddrMode_IndIndeY,
    TEST_AddrMode_Relative
}

test_suite! {
    Suite_UnofficialOps_SLO,
    TEST_SLO_03,
    TEST_SLO_07,
    TEST_SLO_0F,
    TEST_SLO_13,
    TEST_SLO_17,
    TEST_SLO_1B,
    TEST_SLO_1F
}

test_suite! {
    Suite_UnofficialOps_RLA,
    TEST_RLA_23,
    TEST_RLA_27,
    TEST_RLA_2F,
    TEST_RLA_33,
    TEST_RLA_37,
    TEST_RLA_3B,
    TEST_RLA_3F
}

test_suite! {
    Suite_UnofficialOps_SRE,
    TEST_SRE_43,
    TEST_SRE_47,
    TEST_SRE_4F,
    TEST_SRE_53,
    TEST_SRE_57,
    TEST_SRE_5B,
    TEST_SRE_5F
}

test_suite! {
    Suite_UnofficialOps_RRA,
    TEST_RRA_63,
    TEST_RRA_67,
    TEST_RRA_6F,
    TEST_RRA_73,
    TEST_RRA_77,
    TEST_RRA_7B,
    TEST_RRA_7F
}

test_suite! {
    Suite_UnofficialOps__AX,
    TEST_SAX_83,
    TEST_SAX_87,
    TEST_SAX_8F,
    TEST_SAX_97,
    TEST_LAX_A3,
    TEST_LAX_A7,
    TEST_LAX_AF,
    TEST_LAX_B3,
    TEST_LAX_B7,
    TEST_LAX_BF
}

test_suite! {
    Suite_UnofficialOps_DCP,
    TEST_DCP_C3,
    TEST_DCP_C7,
    TEST_DCP_CF,
    TEST_DCP_D3,
    TEST_DCP_D7,
    TEST_DCP_DB,
    TEST_DCP_DF
}

test_suite! {
    Suite_UnofficialOps_ISC,
    TEST_ISC_E3,
    TEST_ISC_E7,
    TEST_ISC_EF,
    TEST_ISC_F3,
    TEST_ISC_F7,
    TEST_ISC_FB,
    TEST_ISC_FF
}

test_suite! {
    Suite_UnofficialOps_SH_,
    TEST_SHA_93 => Err(Error::Code(7)), // DMA timing
    TEST_SHA_9F => Err(Error::Code(7)), // DMA timing
    TEST_SHS_9B => Err(Error::Code(1)),
    TEST_SHY_9C => Err(Error::Code(1)),
    TEST_SHX_9E => Err(Error::Code(1)),
    TEST_LAE_BB
}

test_suite! {
    Suite_UnofficialOps_Immediates,
    TEST_ANC_0B,
    TEST_ANC_2B,
    TEST_ASR_4B,
    TEST_ARR_6B,
    TEST_ANE_8B,
    TEST_LXA_AB,
    TEST_AXS_CB,
    TEST_SBC_EB
}

test_suite! {
    Suite_CPUInterrupts,
    TEST_IFlagLatency => Err(Error::Code(1)),
    TEST_NmiAndBrk => Err(Error::Code(2)),
    TEST_NmiAndIrq => Err(Error::Code(1))
}

test_suite! {
    Suite_DMATests,
    TEST_DMA_Plus_OpenBus => Err(Error::Code(1)),
    TEST_DMA_Plus_2002R => Err(Error::Code(2)),
    TEST_DMA_Plus_2007R => Err(Error::Code(2)),
    TEST_DMA_Plus_2007W => Err(Error::Code(1)),
    TEST_DMA_Plus_4015R => Err(Error::Code(1)),
    TEST_DMA_Plus_4016R => Err(Error::Code(1)),
    TEST_DMABusConflict => Err(Error::Code(2)),
    TEST_DMCDMAPlusOAMDMA => Err(Error::Code(1)),
    TEST_ExplicitDMAAbort => Err(Error::Code(1)),
    TEST_ImplicitDMAAbort => Err(Error::Code(1))
}

test_suite! {
    Suite_APUTiming,
    TEST_APULengthCounter => Err(Error::Code(2)),
    TEST_APULengthTable => Err(Error::Code(1)),
    TEST_FrameCounterIRQ => Err(Error::Code(1)),
    TEST_FrameCounter4Step => Err(Error::Code(1)),
    TEST_FrameCounter5Step => Err(Error::Code(1)),
    TEST_DeltaModulationChannel => Err(Error::Code(1)),
    TEST_APURegActivation => Err(Error::Code(1)),
    TEST_ControllerStrobing => Err(Error::Code(4)),
    TEST_ControllerClocking => Err(Error::Code(2))
}

test_suite! {
    Suite_PowerOnState,
    TEST_PowerOnState_PPU_ResetFlag,
    TEST_PowerOnState_CPU_RAM,
    TEST_PowerOnState_CPU_Registers,
    TEST_PowerOnState_PPU_RAM,
    TEST_PowerOnState_PPU_Palette
}

test_suite! {
    Suite_PPUBehavior,
    TEST_CHRROMIsNotWritable,
    TEST_PPURegMirroring,
    TEST_PPU_Open_Bus,
    TEST_PPUReadBuffer,
    TEST_PaletteRAMQuirks,
    TEST_RenderingFlagBehavior,
    TEST_Rendering2007Read => Err(Error::Code(2)) // Reading from $2007 while rendering is enabled should result in a vertical increment of v.
}

test_suite! {
    Suite_PPUTiming,
    TEST_VBlank_Beginning,
    TEST_VBlank_End,
    TEST_NMI_Control,
    TEST_NMI_Timing,
    TEST_NMI_Suppression,
    TEST_NMI_VBL_End,
    TEST_NMI_Disabled_VBL_Start
}

test_suite! {
    Suite_SpriteZeroHits,
    TEST_SprOverflow_Behavior,
    TEST_Sprite0Hit_Behavior => Err(Error::Code(12)),
    TEST_SuddenlyResizeSprite => Err(Error::Code(4)),
    TEST_ArbitrarySpriteZero => Err(Error::Code(2)),
    TEST_MisalignedOAM_Behavior => Err(Error::Code(1)),
    TEST_Address2004_Behavior => Err(Error::Code(4)),
    TEST_OAM_Corruption => Err(Error::Code(2)),
    TEST_INC4014 => Err(Error::Code(1))
}

test_suite! {
    Suite_PPUMisc,
    TEST_AttributesAsTiles,
    TEST_tRegisterQuirks,
    TEST_StaleBGShiftRegisters => Err(Error::Code(3)), // The background shift registers should not be clocked during H-Blank or F-Blank. After re-enabling rendering, a sprite zero hit should be able to occur entirely on stale background shift register data.
    TEST_BGSerialIn => Err(Error::Code(2)), // Background shift registers should bring in a 1 into bit 0 when shifted. These can be drawn on screen with carefully timed writes to $2001 to enable/disable rendering to skip reloading the shift registers.
    TEST_Scanline0Sprites => Err(Error::Code(2)) // A sprite should be able to be drawn at Y=0 via the pre-render scanline's sprite evaluation with stale secondary OAM data.
}

test_suite! {
    Suite_CPUBehavior2,
    TEST_InstructionTiming => Err(Error::Code(1)), // The DMA should update the data bus.
    TEST_ImpliedDummyRead => Err(Error::Code(2)), // Your emulator did not implement the frame counter interrupt flag properly.
    TEST_BranchDummyRead => Err(Error::Timeout),
    TEST_JSREdgeCases => Err(Error::Code(2)) // Your emulator has incorrect open bus emulation.
}
