//! https://wiki.osdev.org/PIC

use crate::io::{self, Const};

/// IO base address for master PIC
pub const PIC1: Const<0x20> = Const;
/// IO base address for slave PIC
pub const PIC2: Const<0xa0> = Const;
pub const PIC1_COMMAND: Const<0x20> = PIC1;
pub const PIC1_DATA: Const<0x21> = /* PIC1::offset(1) */ Const;
pub const PIC2_COMMAND: Const<0xa0> = PIC2;
pub const PIC2_DATA: Const<0xa1> = /* PIC2::offset(1) */ Const;

/// End-of-interrupt command code
#[allow(dead_code)]
pub const EOI: u8 = 0x20;

/// ICW4 (not) needed
pub const ICW1_ICW4: u8 = 0x01;
/// Single (cascade) mode
#[allow(dead_code)]
pub const ICW1_SINGLE: u8 = 0x02;
/// Call address interval 4 (8)
#[allow(dead_code)]
pub const ICW1_INTERVAL4: u8 = 0x04;
/// Level triggered (edge) mode
#[allow(dead_code)]
pub const ICW1_LEVEL: u8 = 0x08;
/// Initialization - required!
#[allow(dead_code)]
pub const ICW1_INIT: u8 = 0x10;

/// 8086/88 (MCS-80/85) mode
pub const ICW4_8086: u8 = 0x01;
/// Auto (normal) EOI
#[allow(dead_code)]
pub const ICW4_AUTO: u8 = 0x02;
/// Buffered mode/slave
#[allow(dead_code)]
pub const ICW4_BUF_SLAVE: u8 = 0x08;
/// Buffered mode/master
#[allow(dead_code)]
pub const ICW4_BUF_MASTER: u8 = 0x0c;
/// Special fully nested (not)
#[allow(dead_code)]
pub const ICW4_SFNM: u8 = 0x10;

/// Initialize the PIC. This will remap the interrupts and mask all of them.
///
/// They are all masked by default because we don't need them normally. Drivers that
/// do need them should enable them manually (e.g. PIC driver).
///
/// # Safety
///
/// This function must be called exactly once.
pub unsafe fn init() {
    unsafe {
        // Setup PIC
        // ICW1 (allow ICW4 & set PIC to be initialized)
        io::out(PIC1_COMMAND, ICW1_INIT | ICW1_ICW4);
        io::out(PIC2_COMMAND, ICW1_INIT | ICW1_ICW4);
        // ICW2 (map IVT)
        io::out(PIC1_DATA, 240_u8);
        io::out(PIC2_DATA, 248_u8);
        // ICW3 (tell master (PIC1) about slave (PIC2) & vice versa)
        io::out(PIC1_DATA, 4_u8);
        io::out(PIC2_DATA, 2_u8);
        // ICW4 (set 80x86 mode)
        io::out(PIC1_DATA, ICW4_8086);
        io::out(PIC2_DATA, ICW4_8086);
        // Mask all interrupts
        io::out(PIC1_DATA, 0xff_u8);
        io::out(PIC2_DATA, 0xff_u8);
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy)]
enum Ocw3 {
    ReadIrr = 0xa,
    ReadIsr = 0xb,
}

#[allow(dead_code)]
fn irq_reg(ocw3: Ocw3) -> u16 {
    unsafe {
        io::out(PIC1_COMMAND, ocw3 as u8);
        io::out(PIC2_COMMAND, ocw3 as u8);
        u16::from(io::in_::<u8, _>(PIC1_COMMAND)) | (u16::from(io::in_::<u8, _>(PIC2_COMMAND)) << 8)
    }
}
