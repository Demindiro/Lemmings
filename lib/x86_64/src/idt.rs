use crate::mmu::{A4, Phys};
use core::{arch::asm, marker::PhantomData, mem};

pub mod nr {
    macro_rules! def {
		($($name:ident $nr:literal)*) => { $(pub const $name: u8 = $nr;)* };
	}
    def! {
        DIVISION       0
        DEBUG          1
        NMI            2
        BREAKPOINT     3
        OVERFLOW       4
        BOUNDS_RANGE   5
        INVALID_OPCODE 6
        DEV_NOT_AVAIL  7
        DOUBLE_FAULT   8
        _INVALID_09    9
        INVALID_TSS   10
        SEGMENT_NOT_PRESENT 11
        SS_FAULT      12
        GP_FAULT      13
        PAGE_FAULT    14
        _RESERVED_15  15
        X87_FPE       16
        ALIGN_CHECK   17
        MACHINE_CHECK 18
        SIMD_FPE      19
        VIRTE         20
        CPE           21
        _RESERVED_22  22
        _RESERVED_23  23
        _RESERVED_24  24
        _RESERVED_25  25
        _RESERVED_26  26
        _RESERVED_27  27
        HV_INJECTION  28
        VMM_COMM      29
        SECURITY      30
        _RESERVED_31  31
        MAX           32
    }
}

#[repr(C, align(16))]
pub struct IdtEntry {
    offset_low: u16,
    selector: u16,
    ist: u8,
    type_attributes: u8,
    offset_high: u16,
    offset_higher: u32,
    _unused_1: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum Ist {
    N0 = 0,
    N1 = 1,
    N2 = 2,
    N3 = 3,
    N4 = 4,
    N5 = 5,
    N6 = 6,
    N7 = 7,
}

impl IdtEntry {
    /// Disables interrupts on ISR call.
    /// This has nothing to do with the actual type of interrupt or exception.
    const ATTRIBUTE_GATETYPE_INTERRUPT: u8 = 0xe;
    /// Keep interrupts enabled on ISR call.
    /// This has nothing to do with the actual type of interrupt or exception.
    #[allow(dead_code)]
    const ATTRIBUTE_GATETYPE_TRAP: u8 = 0xf;
    const ATTRIBUTE_PRESENT: u8 = 0x80;
    const ATTRIBUTE_DPL: u8 = 0x00;

    pub const fn new(selector: u16, handler: u64, ist: Ist) -> Self {
        Self {
            offset_low: (handler >> 0) as u16,
            selector,
            ist: ist as u8,
            type_attributes: Self::ATTRIBUTE_PRESENT
                | Self::ATTRIBUTE_DPL
                | Self::ATTRIBUTE_GATETYPE_INTERRUPT,
            offset_high: (handler >> 16) as u16,
            offset_higher: (handler >> 32) as u32,
            _unused_1: 0,
        }
    }

    pub fn set_handler(&mut self, handler: *const ()) {
        let handler = handler as u64;
        self.offset_low = (handler >> 0) as u16;
        self.offset_high = (handler >> 16) as u16;
        self.offset_higher = (handler >> 32) as u32;
    }
}

#[repr(transparent)]
pub struct Idt<const MAX: usize> {
    descriptors: [IdtEntry; MAX],
}

impl<const MAX: usize> Idt<MAX> {
    pub const fn new() -> Self {
        const {
            assert!(MAX <= 256, "can't support more than 256 entries");
        }
        Self {
            descriptors: [const { IdtEntry::new(0, 0, Ist::N0) }; MAX],
        }
    }

    pub const fn set(&mut self, index: u8, entry: IdtEntry) {
        self.descriptors[index as usize] = entry;
    }

    /// Set a handler for a particular IRQ.
    ///
    /// This will always use the interrupt gatetype, which disables interrupts on entry.
    ///
    /// It uses [`Ist::N0`], i.e. no switching of the stack.
    ///
    /// It uses the standard GDT layout. See [`crate::gdt`] for more information.
    pub fn set_handler(&mut self, index: u8, handler: *const ()) {
        let entry = IdtEntry::new(crate::gdt::Gdt::KERNEL_CS, handler as _, Ist::N0);
        self.set(index, entry);
    }
}

#[repr(C)]
#[repr(packed)]
pub struct IdtPointer {
    limit: u16,
    offset: u64,
}

impl IdtPointer {
    pub fn new<const MAX: usize>(phys: Phys<A4>) -> Self {
        let limit = const {
            let x = mem::size_of::<Idt<MAX>>() - 1;
            assert!(x <= u16::MAX as usize);
            x as u16
        };
        Self {
            limit,
            offset: phys.into(),
        }
    }

    pub unsafe fn activate(&self) {
        unsafe {
            asm!("lidt [{0}]", in(reg) self, options(readonly, nostack));
        }
    }
}
