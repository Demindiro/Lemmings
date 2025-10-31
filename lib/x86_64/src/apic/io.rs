use super::reg::{RW, Reg};
use crate::mmu::{self, Phys};

/// The default physical address of the I/O APIC.
pub const DEFAULT_ADDR: Phys<mmu::A12> = Phys::new(0xfec00000).expect("address is aligned");

pub struct IoApicHelper<'a> {
    apic: &'a IoApic,
}

#[repr(C)]
pub struct IoApic {
    pub index: Reg<RW>,
    pub data: Reg<RW>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TriggerMode {
    Edge,
    Level,
}

impl<'a> IoApicHelper<'a> {
    pub fn new(apic: &'a IoApic) -> Self {
        Self { apic }
    }

    pub fn irq_offset(irq: u8) -> u32 {
        0x10 + u32::from(irq) * 2
    }

    /// - `irq`: the IRQ of the *device*
    /// - `apic_id`: the Local APIC to send the IRQ to.
    /// - `vector`: the IRQ of the *Local APIC*
    pub unsafe fn set_irq(
        &self,
        irq: u8,
        apic_id: u8,
        vector: u8,
        trigger_mode: TriggerMode,
        mask: bool,
    ) {
        let i = Self::irq_offset(irq);

        unsafe {
            // APIC ID | ...
            self.write(
                i + 1,
                self.read(i + 1) & 0x00ffffff | (u32::from(apic_id) << 24),
            );

            // ... | mask | ... | trigger mode | ... | delivery status | destination | delivery | vector
            let wr = self.read(i + 0) & 0xfffe_0000;
            let wr = wr | u32::from(mask) << 16;
            let wr = wr
                | match trigger_mode {
                    TriggerMode::Edge => 0,
                    TriggerMode::Level => 1,
                } << 15;
            let wr = wr | 0 << 12;
            let wr = wr | 0 << 11;
            let wr = wr | 0b000 << 8;
            let wr = wr | u32::from(vector);
            self.write(i + 0, wr);
        }
    }

    pub unsafe fn mask_irq(&self, irq: u8, enable: bool) {
        let i = Self::irq_offset(irq);
        unsafe {
            self.write(i, self.read(i) & !(1 << 16) | u32::from(enable) << 16);
        }
    }

    /// Read a register from the IoApic
    ///
    /// # Safety
    ///
    /// The register must be valid.
    unsafe fn read(&self, index: u32) -> u32 {
        self.apic.index.set(index);
        self.apic.data.get()
    }

    /// Write to a register of the IoApic
    ///
    /// # Safety
    ///
    /// The register must be valid.
    unsafe fn write(&self, index: u32, value: u32) {
        self.apic.index.set(index);
        self.apic.data.set(value);
    }
}
