#![feature(assert_matches)]

#[allow(unused)]
mod common;

use yaner_cpu::OpCode;

use crate::common::run_test_with_steps;
use std::assert_matches;

enum SuiteTest {
    CpuBehaviour(CpuBehaviour),
}
impl SuiteTest {
    fn suite_pointer(&self) -> u16 {
        match self {
            SuiteTest::CpuBehaviour(_) => 0x8228,
        }
    }
    fn exec_addr(&self) -> u16 {
        match self {
            SuiteTest::CpuBehaviour(test) => *test as u16,
        }
    }
}
#[repr(u16)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum CpuBehaviour {
    RomNotWritable = 0xA14C,
    RamMirroring = 0xA003,
}

#[allow(non_upper_case_globals)]
fn run_accuracy_coin_test(test: SuiteTest) {
    const NMI_Routine: u16 = 0xF6C4;
    const ReadController1: u16 = 0xF90E;
    const RunTest: u16 = 0xF972;
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
                            (OpCode(yaner_cpu::Op::JSR, _), ReadController1)
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
                        nes_state.ram.write_u16(suitePointer, test.suite_pointer());
                        // Test within suite to run
                        nes_state
                            .ram
                            .write_u16(suiteExecPointerList + 2, test.exec_addr());
                        // Where to write the test result
                        nes_state.ram.write_u16(suitePointerList + 2, ResultAddr);

                        // Press A - runs the test
                        nes_state.ram.write_u8(controller_New, 0x80);
                    }
                }
                State::Running => {
                    if let (OpCode(yaner_cpu::Op::JSR, _), RunTest) = nes_state.active_op() {
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
}

#[test]
fn test_rom_not_writable() {
    run_accuracy_coin_test(SuiteTest::CpuBehaviour(CpuBehaviour::RomNotWritable));
}

#[test]
fn test_ram_mirroring() {
    run_accuracy_coin_test(SuiteTest::CpuBehaviour(CpuBehaviour::RamMirroring));
}
