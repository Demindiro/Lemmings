#![no_std]

pub mod apic;
pub mod gdt;
pub mod hpet;
pub mod idt;
pub mod io;
pub mod mmu;
pub mod mtrr;
pub mod pic;
pub mod pit;
pub mod tss;
pub mod uart;

use core::arch::asm;

pub fn halt() {
    unsafe { core::arch::asm!("hlt", options(nomem, nostack, preserves_flags)) };
}

pub unsafe fn enable_interrupts() {
    // TODO is it really nomem/preserves_flags/... if ISRs can do anything?
    unsafe { core::arch::asm!("sti", options(nostack, preserves_flags)) };
}

pub unsafe fn disable_interrupts() {
    unsafe { core::arch::asm!("cli", options(nostack, preserves_flags)) };
}

pub fn current_stack_pointer() -> *mut u8 {
    let dst;
    unsafe {
        core::arch::asm! {
            "mov {dst}, rsp",
            dst = out(reg) dst,
            options(nomem, nostack, preserves_flags, pure)
        }
    }
    dst
}

/// # Safety
///
/// TODO
pub unsafe fn set_fs(x: *mut u8) {
    unsafe { asm!("wrfsbase {}", in(reg) x, options(nomem, nostack, preserves_flags)) };
}

/// # Safety
///
/// TODO
pub unsafe fn set_gs(x: *mut u8) {
    unsafe { asm!("wrgsbase {}", in(reg) x, options(nomem, nostack, preserves_flags)) };
}

pub fn fs() -> *mut u8 {
    let x;
    unsafe { asm!("rdfsbase {}", out(reg) x, options(nomem, nostack, pure, preserves_flags)) };
    x
}

pub fn gs() -> *mut u8 {
    let x;
    unsafe { asm!("rdgsbase {}", out(reg) x, options(nomem, nostack, pure, preserves_flags)) };
    x
}
