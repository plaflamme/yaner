#![feature(assert_matches)]
#![allow(non_upper_case_globals, non_snake_case)]

#[allow(unused)]
mod common;

use yaner_cpu::OpCode;

use crate::common::run_test_with_steps;
use std::{assert_matches, collections::HashMap, path::PathBuf};
use test_case::test_case;

struct AccuracyCoin {
    root: PathBuf,
    listing: HashMap<String, u16>,
}

impl AccuracyCoin {
    fn new(root: impl Into<PathBuf>) -> Result<Self, Box<dyn std::error::Error>> {
        let root = root.into();
        let fns = root.join("AccuracyCoin.fns");
        let fns = std::fs::read_to_string(fns)?;
        let listing = fns
            .lines()
            .map(|l| l.trim())
            .filter(|l| !l.starts_with(";"))
            .map(|l| {
                let parse = || -> Result<(String, u16), Box<dyn std::error::Error>> {
                    let (fns, addr) = l.split_once("=").ok_or(format!("Invalid line {l}"))?;
                    let addr = u16::from_str_radix(addr.trim().trim_start_matches('$'), 16)?;
                    Ok((fns.trim().to_string(), addr))
                };
                parse()
            })
            .collect::<Result<HashMap<String, u16>, Box<dyn std::error::Error>>>()?;

        Ok(Self { root, listing })
    }

    fn addr(&self, label: &str) -> Result<u16, String> {
        self.listing
            .get(label)
            .copied()
            .ok_or_else(|| format!("Cannot find address of label {label}"))
    }

    fn rom(&self) -> PathBuf {
        self.root.join("AccuracyCoin.nes")
    }
}

fn run_accuracy_coin_test(suite: &str, test: &str) -> Result<(), Box<dyn std::error::Error>> {
    let accuracy_coin = AccuracyCoin::new("roms/AccuracyCoin")?;
    let suite_pointer = accuracy_coin.addr(suite)?;
    let test_exec_pointer = accuracy_coin.addr(test)?;

    let NMI_Routine = accuracy_coin.addr("NMI_Routine")?;
    let ReadController1 = accuracy_coin.addr("ReadController1")?;
    let RunTest = accuracy_coin.addr("RunTest")?;

    // TODO: read out of .asm
    const controller_New: u16 = 0x0019;
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
    run_test_with_steps(
        accuracy_coin.rom(),
        None,
        |steps| steps.step_cpu().map(|_| ()),
        |nes_state| {
            match state {
                State::Setup => {
                    if nes_state.cpu.active_pc == NMI_Routine {
                        // Check our assumption before setting the breakpoint
                        assert_matches!(
                            nes_state.active_op(),
                            (OpCode(yaner_cpu::Op::JSR, _), addr) if addr == ReadController1
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
                    if let (OpCode(yaner_cpu::Op::JSR, _), addr) = nes_state.active_op()
                        && addr == RunTest
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
            assert_eq!(nes_state.ram.read_u8(ResultAddr), 1)
        },
    );
    Ok(())
}
fn run_accuracy_coin_test_with_timeout(suite: &'static str, test: &'static str) {
    let (sender, receiver) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        run_accuracy_coin_test(suite, test).unwrap();
        sender.send(()).unwrap();
    });
    receiver
        .recv_timeout(std::time::Duration::from_secs(15))
        .unwrap();
}

macro_rules! test_suite {
    ($suite_name: ident, $($test_name: ident $(=> $more:tt)?),+) => {

        $(
            const $test_name: &str = stringify!($test_name);
        )+

        $(#[test_case($test_name $( => $more )? )])+
        fn $suite_name(test: &'static str) {
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
    TEST_BFlag => panics,
    TEST_DummyReads,
    TEST_DummyWrites,
    TEST_OpenBus => panics,
    TEST_AllNOPs => panics
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
    TEST_SHA_93 => panics,
    TEST_SHA_9F => panics,
    TEST_SHS_9B => panics,
    TEST_SHY_9C => panics,
    TEST_SHX_9E => panics,
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
    TEST_IFlagLatency => panics,
    TEST_NmiAndBrk => panics,
    TEST_NmiAndIrq => panics
}

test_suite! {
    Suite_DMATests,
    TEST_DMA_Plus_OpenBus => panics,
    TEST_DMA_Plus_2002R => panics,
    TEST_DMA_Plus_2007R => panics,
    TEST_DMA_Plus_2007W => panics,
    TEST_DMA_Plus_4015R => panics,
    TEST_DMA_Plus_4016R => panics,
    TEST_DMABusConflict => panics,
    TEST_DMCDMAPlusOAMDMA => panics,
    TEST_ExplicitDMAAbort => panics,
    TEST_ImplicitDMAAbort => panics
}

test_suite! {
    Suite_APUTiming,
    TEST_APULengthCounter => panics,
    TEST_APULengthTable => panics,
    TEST_FrameCounterIRQ => panics,
    TEST_FrameCounter4Step => panics,
    TEST_FrameCounter5Step => panics,
    TEST_DeltaModulationChannel => panics,
    TEST_APURegActivation => panics,
    TEST_ControllerStrobing => panics,
    TEST_ControllerClocking => panics
}

test_suite! {
    Suite_PowerOnState,
    TEST_PowerOnState_PPU_ResetFlag => panics,
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
    TEST_PaletteRAMQuirks => panics,
    TEST_RenderingFlagBehavior,
    TEST_Rendering2007Read => panics
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
    TEST_Sprite0Hit_Behavior => panics,
    TEST_SuddenlyResizeSprite => panics,
    TEST_ArbitrarySpriteZero => panics,
    TEST_MisalignedOAM_Behavior => panics,
    TEST_Address2004_Behavior => panics,
    TEST_OAM_Corruption => panics,
    TEST_INC4014 => panics
}

test_suite! {
    Suite_PPUMisc,
    TEST_AttributesAsTiles,
    TEST_tRegisterQuirks,
    TEST_StaleBGShiftRegisters => panics,
    TEST_BGSerialIn => panics,
    TEST_Scanline0Sprites => panics
}

test_suite! {
    Suite_CPUBehavior2,
    TEST_InstructionTiming => panics,
    TEST_ImpliedDummyRead => panics,
    TEST_BranchDummyRead => panics,
    TEST_JSREdgeCases => panics
}
