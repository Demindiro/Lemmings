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

macro_rules! reg {
    ([get] $($fn:ident $instr:literal)*) => {
        $(
        pub fn $fn() -> u64 {
            let x;
            unsafe { core::arch::asm!($instr, out(reg) x, options(nomem, nostack, pure, preserves_flags)) };
            x
        }
        )*
    };
    ([set] $($fn:ident $instr:literal)*) => {
        $(
        /// # Safety
        ///
        /// TODO
        pub unsafe fn $fn(value: u64) {
            unsafe { core::arch::asm!($instr, in(reg) value, options(nostack, preserves_flags)) }
        }
        )*
    };
    ([update] $($fn:ident $get_fn:ident $set_fn:ident)*) => {
        $(
        /// # Safety
        ///
        /// TODO
        pub unsafe fn $fn<F>(f: F)
        where
            F: FnOnce(u64) -> u64,
        {
            unsafe { $set_fn((f)($get_fn())) }
        }
        )*
    };
    ($([$reg:ident $get_instr:literal $set_instr:literal] $($cst:ident = $val:expr;)*)*) => {$(
        pub mod $reg {
            $(pub const $cst: u64 = $val;)*

            pub fn get() -> u64 {
                let x;
                unsafe { core::arch::asm!($get_instr, out(reg) x, options(nomem, nostack, pure, preserves_flags)) };
                x
            }

            /// # Safety
            ///
            /// TODO
            pub unsafe fn set(value: u64) {
                unsafe { core::arch::asm!($set_instr, in(reg) value, options(nostack, preserves_flags)) }
            }

            /// # Returns
            ///
            /// The value *before* updating.
            ///
            /// # Safety
            ///
            /// TODO
            pub unsafe fn update<F>(f: F) -> u64
            where
                F: FnOnce(u64) -> u64,
            {
                let x = get();
                unsafe { set(f(x)); }
                x
            }
        }
    )*}
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

reg! {
    [cr0 "mov {}, cr0" "mov cr0, {}"]
        WRITE_PROTECT = 1 << 16;
    [cr2 "mov {}, cr2" "mov cr2, {}"]
    [cr3 "mov {}, cr3" "mov cr3, {}"]
    [cr4 "mov {}, cr4" "mov cr4, {}"]
        FSGSBASE = 1 << 16;
    [cr8 "mov {}, cr8" "mov cr8, {}"]
}
