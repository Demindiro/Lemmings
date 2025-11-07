use super::*;

pub enum Capability<'a> {
    Msi(&'a Msi),
    Vendor(&'a Vendor),
    MsiX(&'a MsiX),
}

#[repr(C)]
pub struct Msi {
    common: super::Capability,
    message_control: VolatileCell<MsiMessageControl>,
    message_address_low: VolatileCell<u32le>,
    message_address_high: VolatileCell<u32le>,
    message_data: VolatileCell<u16le>,
    _reserved: [VolatileCell<u8>; 2],
    mask: VolatileCell<u32le>,
    pending: VolatileCell<u32le>,
}

#[derive(Copy, Clone, Debug)]
pub enum MsiInterrupts {
    N1,
    N2,
    N4,
    N8,
    N16,
    N32,
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct MsiMessageControl(u16le);

#[repr(C)]
pub struct Vendor {
    common: super::Capability,
    length: VolatileCell<u8>,
}

#[repr(C)]
pub struct MsiX {
    common: super::Capability,
    message_control: VolatileCell<MsiXMessageControl>,
    table_bir_offset: VolatileCell<u32le>,
    pending_bit_bir_offset: VolatileCell<u32le>,
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct MsiXMessageControl(u16le);

impl Msi {
    get_volatile!(message_control -> MsiMessageControl);
    set_volatile!(set_message_control: message_control <- MsiMessageControl);

    pub fn message_address(&self) -> u64 {
        let f = |n: &VolatileCell<u32le>| u64::from(u32::from(n.get()));
        f(&self.message_address_low) | f(&self.message_address_high) << 32
    }

    pub fn set_message_address(&self, address: u64) {
        self.message_address_low.set((address as u32).into());
        self.message_address_high
            .set(((address >> 32) as u32).into());
    }

    get_volatile!(message_data -> u16);
    set_volatile!(set_message_data: message_data <- u16);
    get_volatile!(mask -> u32);
    set_volatile!(set_mask: mask <- u32);
    get_volatile!(pending -> u32);
}

impl MsiMessageControl {
    pub fn enable(&self) -> bool {
        self.0 & 1 != 0
    }

    pub fn set_enable(&mut self, enable: bool) {
        self.0 &= !1;
        self.0 |= u16::from(enable);
    }

    pub fn multiple_message_capable(&self) -> Option<MsiInterrupts> {
        MsiInterrupts::from_raw(u16::from(self.0) >> 1 & 0x111)
    }

    pub fn multiple_message_enable(&self) -> Option<MsiInterrupts> {
        MsiInterrupts::from_raw(u16::from(self.0) >> 4 & 0x111)
    }

    pub fn set_multiple_message_enable(&mut self, count: MsiInterrupts) {
        self.0 &= !(7 << 4);
        self.0 |= (count as u16) << 4;
    }

    pub fn address_64(&self) -> bool {
        self.0 & 1 << 7 != 0
    }

    pub fn per_vector_masking(&self) -> bool {
        self.0 & 1 << 8 != 0
    }
}

impl MsiInterrupts {
    fn from_raw(n: u16) -> Option<Self> {
        Some(match n {
            0 => Self::N1,
            1 => Self::N2,
            2 => Self::N4,
            3 => Self::N8,
            4 => Self::N16,
            5 => Self::N32,
            _ => return None,
        })
    }
}

impl fmt::Debug for Msi {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(stringify!(Msi))
            .field("common", &self.common)
            .field("message_control", &self.message_control())
            .field(
                "message_address",
                &format_args!("0x{:016x}", self.message_address()),
            )
            .field(
                "message_data",
                &format_args!("0x{:04x}", self.message_data()),
            )
            .field("mask", &format_args!("0x{:04x}", self.mask()))
            .field("pending", &format_args!("0x{:04x}", self.pending()))
            .finish()
    }
}

impl fmt::Debug for MsiMessageControl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(stringify!(MsiMessageControl))
            .field("enable", &self.enable())
            .field("multiple_message_capable", &self.multiple_message_capable())
            .field("multiple_message_enable", &self.multiple_message_enable())
            .field("address_64", &self.address_64())
            .field("per_vector_masking", &self.per_vector_masking())
            .finish()
    }
}

impl Vendor {
    get_volatile!(length -> u8);
}

impl MsiX {
    get_volatile!(message_control -> MsiXMessageControl);
    set_volatile!(set_message_control: message_control <- MsiXMessageControl);

    #[inline]
    pub fn table(&self) -> (u32, u8) {
        let v = u32::from(self.table_bir_offset.get());
        (v & !0x7, (v & 0x7) as u8)
    }

    #[inline]
    pub fn pending(&self) -> (u32, u8) {
        let v = u32::from(self.pending_bit_bir_offset.get());
        (v & !0x7, (v & 0x7) as u8)
    }
}

impl MsiXMessageControl {
    const ENABLE: u16le = u16le::new(1 << 15);

    #[inline]
    pub fn enable(&self) -> bool {
        u16::from(self.0 & Self::ENABLE) > 0u16
    }

    #[inline]
    pub fn set_enable(&mut self, value: bool) {
        if value {
            self.0 |= Self::ENABLE;
        } else {
            self.0 &= !Self::ENABLE;
        }
    }

    #[inline]
    pub fn function_mask(&self) -> bool {
        u16::from(self.0) & (1 << 14) > 0
    }

    #[inline]
    pub fn table_size(&self) -> u16 {
        u16::from(self.0) & 0x3ff
    }
}

impl fmt::Debug for Capability<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Msi(m) => m.fmt(f),
            Self::Vendor(m) => m.fmt(f),
            Self::MsiX(m) => m.fmt(f),
        }
    }
}

impl fmt::Debug for MsiX {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (table_offset, table_bir) = self.table();
        let (pending_offset, pending_bir) = self.pending();
        f.debug_struct(stringify!(MsiX))
            .field("common", &self.common)
            .field("message_control", &self.message_control())
            .field("table_offset", &format_args!("0x{:04x}", table_offset))
            .field("table_bir", &table_bir)
            .field("pending_offset", &format_args!("0x{:04x}", pending_offset))
            .field("pending_bir", &pending_bir)
            .finish()
    }
}

impl fmt::Debug for MsiXMessageControl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(stringify!(MsiXMessageControl))
            .field("enable", &self.enable())
            .field("function_mask", &self.function_mask())
            .field("table_size", &self.table_size())
            .finish()
    }
}

impl fmt::Debug for Vendor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(stringify!(Vendor))
            .field("common", &self.common)
            .field("length", &self.length())
            .finish_non_exhaustive()
    }
}
