#![feature(assert_matches)]

#[allow(unused)]
mod common;

use yaner_cpu::OpCode;

use crate::common::run_test_with_steps;
use std::{assert_matches, collections::HashMap, path::PathBuf};
use test_case::test_case;

struct AccuracyCoin {
    listing: HashMap<String, u16>,
}

impl AccuracyCoin {
    fn new(root: impl Into<PathBuf>) -> Result<Self, Box<dyn std::error::Error>> {
        let fns = root.into().join("AccuracyCoin.fns");
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

        Ok(Self { listing })
    }

    fn addr(&self, label: &str) -> Result<u16, String> {
        self.listing
            .get(label)
            .copied()
            .ok_or_else(|| format!("Cannot find address of label {label}"))
    }
}

#[allow(non_upper_case_globals)]
#[allow(non_snake_case)]
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
        "roms/AccuracyCoin/AccuracyCoin.nes",
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

#[test_case("TEST_ROMnotWritable")]
#[test_case("TEST_RamMirroring")]
#[test_case("Test_ProgramCounter_Wraparound")]
#[test_case("TEST_DecimalFlag")]
#[test_case("TEST_BFlag")]
#[test_case("TEST_DummyReads")]
#[test_case("TEST_DummyWrites")]
#[test_case("TEST_OpenBus")]
#[test_case("TEST_AllNOPs")]
fn test_cpu_behaviour(test: &str) -> Result<(), Box<dyn std::error::Error>> {
    run_accuracy_coin_test("Suite_CPUBehavior", test)
}
