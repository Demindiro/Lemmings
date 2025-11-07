//! Library for iterating and interacting with PCI and PCIe devices.
//!
//! ## References
//!
//! [Pci on OSDev wiki][osdev pci]
//!
//! [osdev pci]: https://wiki.osdev.org/Pci

#![no_std]
#![forbid(unconditional_recursion, unsafe_op_in_unsafe_fn, unused_must_use)]

macro_rules! get_volatile {
    ($f:ident -> $t:ty) => {
        pub fn $f(&self) -> $t {
            self.$f.get().into()
        }
    };
}

macro_rules! set_volatile {
    ($fn:ident : $f:ident <- $t:ty) => {
        pub fn $fn(&self, value: $t) {
            self.$f.set(value.into())
        }
    };
}

pub mod capability;
pub mod msix;

use core::{
    cell::Cell, convert::TryInto, fmt, marker::PhantomData, num::NonZeroU32, ops, ptr::NonNull,
};
use lemmings_endian::{u16le, u32le};
use lemmings_volatile::VolatileCell;

pub const BAR_IO_SPACE: u32 = 1;
pub const BAR_TYPE_MASK: u32 = 0x6;

/// Bus-Device-Function
#[derive(Clone, Copy)]
pub struct Bdf(u16);

/// Representation of a base address (BAR).
///
/// I/O bar layout:
///
/// ```
/// +------------------------+----------+----------+
/// | 31 - 2                 | 1        | 0        |
/// +------------------------+----------+----------+
/// | 4 byte aligned address | reserved | always 0 |
/// +------------------------+----------+----------+
/// ```
///
/// MMIO bar layout:
///
/// ```
/// +-------------------------+--------------+-------+----------+
/// | 31 - 4                  | 3            | 1 - 2 | 0        |
/// +-------------------------+--------------+-------+----------+
/// | 16 byte aligned address | prefetchable | type  | always 1 |
/// +-------------------------+--------------+-------+----------+
/// ```
#[repr(transparent)]
pub struct BaseAddress(VolatileCell<u32le>);

/// A BAR in a more friendly format.
pub enum ParsedBaseAddress {
    IO32 { address: u32 },
    MMIO32 { address: u32, prefetchable: bool },
    MMIO64 { address: u64, prefetchable: bool },
}

/// Enum of possible headers.
#[derive(Clone, Copy, Debug)]
pub enum Header<'a> {
    H0(&'a Header0),
    H1(&'a Header1),
    Unknown(&'a HeaderCommon),
}

/// Common header fields.
#[repr(C)]
pub struct HeaderCommon {
    vendor_id: VolatileCell<u16le>,
    device_id: VolatileCell<u16le>,

    pub command: VolatileCell<u16le>,
    pub status: VolatileCell<u16le>,

    revision_id: VolatileCell<u8>,
    programming_interface: VolatileCell<u8>,
    subclass: VolatileCell<u8>,
    class_code: VolatileCell<u8>,

    cache_line_size: VolatileCell<u8>,
    latency_timer: VolatileCell<u8>,
    header_type: VolatileCell<u8>,
    bist: VolatileCell<u8>,
}

/// Header type 0x00
#[repr(C, align(4096))]
pub struct Header0 {
    pub common: HeaderCommon,

    pub base_address: [BaseAddress; 6],

    cardbus_cis_pointer: VolatileCell<u32le>,

    subsystem_vendor_id: VolatileCell<u16le>,
    subsystem_id: VolatileCell<u16le>,

    expansion_rom_base_address: VolatileCell<u32le>,

    capabilities_pointer: VolatileCell<u8>,

    _reserved: [VolatileCell<u8>; 7],

    pub interrupt_line: VolatileCell<u8>,
    pub interrupt_pin: VolatileCell<u8>, // TODO is pub a good or bad idea?
    min_grant: VolatileCell<u8>,
    max_latency: VolatileCell<u8>,
}

/// Header type 0x01 (PCI-to-PCI bridge)
#[repr(C, align(4096))]
pub struct Header1 {
    pub common: HeaderCommon,

    pub base_address: [BaseAddress; 2],

    primary_bus_number: VolatileCell<u8>,
    secondary_bus_number: VolatileCell<u8>,
    subordinate_bus_number: VolatileCell<u8>,
    secondary_latency_timer: VolatileCell<u8>,

    io_base: VolatileCell<u8>,
    io_limit: VolatileCell<u8>,
    secondary_status: VolatileCell<u16le>,

    memory_base: VolatileCell<u16le>,
    memory_limit: VolatileCell<u16le>,

    prefetchable_memory_base: VolatileCell<u16le>,
    prefetchable_memory_limit: VolatileCell<u16le>,

    prefetchable_base_upper_32_bits: VolatileCell<u32le>,
    prefetchable_limit_upper_32_bits: VolatileCell<u32le>,

    io_base_upper_16_bits: VolatileCell<u16le>,
    io_limit_upper_16_bits: VolatileCell<u16le>,

    capabilities_pointer: VolatileCell<u8>,

    _reserved: [VolatileCell<u8>; 3],

    expansion_rom_base_address: VolatileCell<u32le>,

    interrupt_line: VolatileCell<u8>,
    interrupt_pin: VolatileCell<u8>,
    bridge_control: VolatileCell<u16le>,
}

const _: () = assert!(core::mem::size_of::<Header0>() == 4096);
const _: () = assert!(core::mem::size_of::<Header1>() == 4096);

#[repr(C)]
pub struct Capability {
    id: VolatileCell<u8>,
    next: VolatileCell<u8>,
}

#[derive(Clone, Debug)]
pub enum InvalidBdf {
    DeviceOutOfRange(u8),
    FunctionOutOfRange(u8),
}

impl Bdf {
    #[inline]
    pub fn new(bus: u8, device: u8, function: u8) -> Result<Self, InvalidBdf> {
        if device >= 32 {
            return Err(InvalidBdf::FunctionOutOfRange(device));
        }
        if function >= 8 {
            return Err(InvalidBdf::FunctionOutOfRange(function));
        }
        let f = |x, s| u16::from(x) << s;
        Ok(Self(f(bus, 8) | f(device, 3) | f(function, 0)))
    }

    pub fn index(&self) -> u16 {
        self.0
    }

    pub fn bus(&self) -> u8 {
        (self.0 >> 8) as u8
    }

    pub fn device(&self) -> u8 {
        (self.0 >> 3) as u8 % 32
    }

    pub fn function(&self) -> u8 {
        (self.0 >> 0) as u8 % 8
    }
}

impl BaseAddress {
    /// Check if a BAR value indicates an MMIO BAR.
    pub fn is_mmio(value: u32) -> bool {
        value & 1 == 0
    }

    /// Check if a BAR value indicates an I/O BAR.
    pub fn is_io(value: u32) -> bool {
        value & 1 == 1
    }

    /// Check if a BAR value indicates a 32 bit BAR.
    pub fn is_64bit(value: u32) -> bool {
        value & 0x6 == 0x4
    }

    /// Check if a BAR value indicates a 64 bit BAR.
    pub fn is_32bit(value: u32) -> bool {
        value & 0x6 == 0x0
    }

    /// Return the physical address the BAR(s) point(s) to. This may be a 64 bit address
    pub fn address<F>(lower: u32, upper: F) -> Option<u64>
    where
        F: FnOnce() -> Option<u32>,
    {
        if Self::is_64bit(lower) {
            Some(u64::from(lower & !0xf) | u64::from(upper()?) << 32)
        } else if Self::is_mmio(lower) {
            Some(u64::from(lower & !0xf))
        } else {
            None
        }
    }

    /// If set, reads won't have any side effects. This is useful to make better use of caching.
    pub fn is_prefetchable(value: u32) -> bool {
        value & 0x8 > 0
    }

    /// Get the full address one or two BARs point to. This may be 64-bit.
    ///
    /// Returns `None` if the BAR is invalid.
    pub fn full_base_address(bars: &[Self], index: usize) -> Option<ParsedBaseAddress> {
        let low = bars.get(index)?.0.get().into();
        if BaseAddress::is_io(low) {
            Some(ParsedBaseAddress::IO32 {
                address: low & !0x3,
            })
        } else if BaseAddress::is_32bit(low) {
            Some(ParsedBaseAddress::MMIO32 {
                address: low & !0xf,
                prefetchable: BaseAddress::is_prefetchable(low),
            })
        } else if BaseAddress::is_64bit(low) {
            Some(ParsedBaseAddress::MMIO64 {
                address: u64::from(low & !0xf)
                    | u64::from(u32::from(bars.get(index + 1)?.0.get())) << 32,
                prefetchable: BaseAddress::is_prefetchable(low),
            })
        } else {
            None
        }
    }

    /// Return the size of the memory area a BAR points to.
    ///
    /// This dirties the register, so the original value must be restored afterwards (if any).
    ///
    /// If the returned size is None, the original value does not need to be restored.
    ///
    /// # Returns
    ///
    /// The size as well as the original value. The size is None if the masked value is 0.
    #[must_use = "this call dirties the register"]
    pub fn size(&self) -> (Option<NonZeroU32>, u32) {
        let og = self.get();
        let mask = match Self::is_mmio(og) {
            true => !0xf,
            false => !0x3,
        };
        self.set(u32::MAX);
        let masked = self.get() & mask;
        (
            (masked != 0).then(|| NonZeroU32::new(!masked + 1).unwrap()),
            og,
        )
    }

    /// Return the raw value.
    #[must_use = "volatile loads cannot be optimized out"]
    pub fn get(&self) -> u32 {
        self.0.get().into()
    }

    /// Set the raw value.
    pub fn set(&self, value: u32) {
        self.0.set(value.into());
    }
}

impl ParsedBaseAddress {
    #[inline]
    pub fn try_as_mmio(&self) -> Option<u64> {
        match self {
            Self::IO32 { .. } => None,
            Self::MMIO32 { address, .. } => Some(u64::from(*address)),
            Self::MMIO64 { address, .. } => Some(*address),
        }
    }
}

impl HeaderCommon {
    /// Flag used to enable MMIO
    pub const COMMAND_MMIO_MASK: u16 = 0x2;
    /// Flag used to toggle bus mastering.
    pub const COMMAND_BUS_MASTER_MASK: u16 = 0x4;
    /// Flag used to disable interrupts.
    pub const COMMAND_INTERRUPT_DISABLE: u16 = 1 << 10;

    get_volatile!(vendor_id -> u16);
    get_volatile!(device_id -> u16);
    get_volatile!(command -> u16);
    get_volatile!(status -> u16);
    get_volatile!(revision_id -> u8);
    get_volatile!(programming_interface -> u8);
    get_volatile!(subclass -> u8);
    get_volatile!(class_code -> u8);
    get_volatile!(cache_line_size -> u8);
    get_volatile!(latency_timer -> u8);
    get_volatile!(header_type -> u8);
    get_volatile!(bist -> u8);

    pub fn has_capabilities(&self) -> bool {
        self.status() & (1 << 4) > 0
    }

    /// Set the flags in the command register.
    pub fn set_command(&self, flags: u16) {
        self.command.set(flags.into());
    }
}

impl Header0 {
    pub const BASE_ADDRESS_COUNT: u8 = 6;

    /// Return the capability structures attached to this header.
    pub fn capabilities<'a>(&'a self) -> CapabilityIter<'a> {
        CapabilityIter {
            marker: PhantomData,
            next: self.common.has_capabilities().then(|| unsafe {
                let next =
                    (self as *const _ as *const u8).add(self.capabilities_pointer.get().into());
                NonNull::new_unchecked(next as *mut Capability).cast()
            }),
        }
    }

    get_volatile!(cardbus_cis_pointer -> u32);
    get_volatile!(subsystem_vendor_id -> u16);
    get_volatile!(subsystem_id -> u16);
    get_volatile!(expansion_rom_base_address -> u32);
    get_volatile!(capabilities_pointer -> u8);
    get_volatile!(interrupt_line -> u8);
    get_volatile!(interrupt_pin -> u8);
    get_volatile!(min_grant -> u8);
    get_volatile!(max_latency -> u8);

    pub fn base_address(&self, index: usize) -> u32 {
        self.base_address[usize::from(index)].get().into()
    }

    pub fn set_base_address(&self, index: usize, value: u32) {
        self.base_address[usize::from(index)].set(value.into());
    }

    pub fn set_command(&self, value: u16) {
        self.common.set_command(value);
    }

    /// Get the full address one or two BARs point to. This may be 64-bit.
    ///
    /// Returns `None` if the BAR is invalid.
    pub fn full_base_address(&self, index: usize) -> Option<ParsedBaseAddress> {
        BaseAddress::full_base_address(&self.base_address, index)
    }

    /// Read the status register
    pub fn status(&self) -> u16 {
        self.common.status()
    }
}

impl Header1 {
    /// Return the capability structures attached to this header.
    pub fn capabilities<'a>(&'a self) -> CapabilityIter<'a> {
        CapabilityIter {
            marker: PhantomData,
            next: self.common.has_capabilities().then(|| unsafe {
                let next =
                    (self as *const _ as *const u8).add(self.capabilities_pointer.get().into());
                NonNull::new_unchecked(next as *mut Capability).cast()
            }),
        }
    }

    /// Get the full address one or two BARs point to. This may be 64-bit.
    ///
    /// Returns `None` if the BAR is invalid.
    pub fn full_base_address(&self, index: usize) -> Option<ParsedBaseAddress> {
        BaseAddress::full_base_address(&self.base_address, index)
    }
}

impl ops::Deref for Header0 {
    type Target = HeaderCommon;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl ops::Deref for Header1 {
    type Target = HeaderCommon;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

macro_rules! common {
    ($name:ident $ty:ty) => {
        pub fn $name(&self) -> $ty {
            self.common().$name.get().into()
        }
    };
}

impl<'a> Header<'a> {
    pub fn common(&self) -> &'a HeaderCommon {
        match self {
            Self::H0(h) => &h.common,
            Self::H1(h) => &h.common,
            Self::Unknown(hc) => hc,
        }
    }

    common!(vendor_id u16);
    common!(device_id u16);
    common!(programming_interface u8);
    common!(subclass u8);
    common!(class_code u8);

    /// Return the capability structures attached to this header.
    pub fn capabilities(&self) -> CapabilityIter<'a> {
        match self {
            Self::H0(h) => h.capabilities(),
            Self::H1(h) => h.capabilities(),
            Self::Unknown(_) => CapabilityIter {
                marker: PhantomData,
                next: None,
            },
        }
    }

    pub fn base_addresses(&self) -> &[BaseAddress] {
        match self {
            Self::H0(h) => &h.base_address[..],
            Self::H1(h) => &h.base_address[..],
            Self::Unknown(_) => &[],
        }
    }

    pub fn full_base_address(&self, index: usize) -> Option<ParsedBaseAddress> {
        BaseAddress::full_base_address(self.base_addresses(), index)
    }

    pub fn header_type(&self) -> u8 {
        self.common().header_type.get()
    }

    pub fn set_command(&self, flags: u16) {
        self.common().set_command(flags);
    }

    /// Read the status register
    pub fn status(&self) -> u16 {
        self.common().status()
    }

    pub unsafe fn from_raw(address: *const ()) -> Self {
        unsafe {
            let hc = &*(address as *const HeaderCommon);
            match hc.header_type.get() & 0x7f {
                0 => Self::H0(&*(address as *const Header0)),
                1 => Self::H1(&*(address as *const Header1)),
                _ => Self::Unknown(hc),
            }
        }
    }
}

impl Capability {
    /// Return the capability ID.
    pub fn id(&self) -> u8 {
        self.id.get()
    }

    /// Return a reference to data that is located right after the capability header.
    ///
    /// ## Safety
    ///
    /// It is up to the caller to ensure that the data actually exists and won't go out of bounds.
    pub unsafe fn data<'a, T>(&'a self) -> &'a T {
        unsafe { NonNull::from(self).cast().as_ref() }
    }

    /// Cast this capability to a concrete type if the ID is recognized.
    pub fn downcast<'a>(&'a self) -> Option<capability::Capability<'a>> {
        unsafe {
            use capability::*;
            match self.id() {
                0x_5 => Some(Capability::Msi(&*(self as *const _ as *const _))),
                0x_9 => Some(Capability::Vendor(&*(self as *const _ as *const _))),
                0x11 => Some(Capability::MsiX(&*(self as *const _ as *const _))),
                _ => None,
            }
        }
    }
}

pub struct CapabilityIter<'a> {
    next: Option<NonNull<Capability>>,
    marker: PhantomData<&'a Capability>,
}

/// Representation of a Pci MMIO area
#[repr(C, align(4096))]
pub struct Pci([[VolatileCell<u8>; 4096]; 1 << 16]);

const _: () = assert!(core::mem::size_of::<Pci>() == 1 << (12 + 16));

impl Pci {
    /// Return a reference to the configuration header for a function.
    pub fn get(&self, bdf: Bdf) -> Header<'_> {
        unsafe {
            let h = NonNull::from(self).byte_add(usize::from(bdf.index()) << 12);
            let hc = h.cast::<HeaderCommon>().as_ref();
            match hc.header_type.get() & 0x7f {
                0 => Header::H0(h.cast().as_ref()),
                1 => Header::H1(h.cast().as_ref()),
                _ => Header::Unknown(hc),
            }
        }
    }

    /// Perform internal iteration on all *valid* devices.
    // because external is a pain in the ass.
    pub fn list<F>(&self, mut f: F)
    where
        F: FnMut(Bdf, Header<'_>),
    {
        self.scan_bus(0, &mut f);
    }

    fn scan_bus<F>(&self, bus: u8, f: &mut F)
    where
        F: FnMut(Bdf, Header<'_>),
    {
        for dev in 0..32 {
            let bdf = Bdf::new(bus, dev, 0).expect("dev number in range");
            self.scan_dev(bdf, f);
        }
    }

    fn scan_dev<F>(&self, bdf: Bdf, f: &mut F)
    where
        F: FnMut(Bdf, Header<'_>),
    {
        let hdr = self.get(bdf);
        if hdr.vendor_id() != 0xffff {
            (f)(bdf, hdr);
        }
        // TODO scan other buses
    }
}

impl fmt::Debug for Bdf {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:02x}:{:02x}.{:x}",
            self.bus(),
            self.device(),
            self.function()
        )
    }
}

impl fmt::Debug for HeaderCommon {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(stringify!(HeaderCommon))
            .field("vendor_id", &format_args!("0x{:04x}", self.vendor_id()))
            .field("device_id", &format_args!("0x{:04x}", self.device_id()))
            .field("command", &format_args!("0b{:016b}", self.command()))
            .field("status", &format_args!("0b{:016b}", self.status()))
            .field("revision_id", &self.revision_id())
            .field("programming_interface", &self.programming_interface())
            .field("subclass", &self.subclass())
            .field("class_code", &self.class_code())
            .field("cache_line_size", &self.cache_line_size())
            .field("latency_timer", &self.latency_timer())
            .field("header_type", &self.header_type())
            .field("bist", &self.bist())
            .finish()
    }
}

impl fmt::Debug for Header0 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(stringify!(Header0))
            .field("common", &self.common)
            .field("base_address", &self.base_address)
            .field("cardbus_cis_pointer", &self.cardbus_cis_pointer())
            .field(
                "subsystem_vendor_id",
                &format_args!("0x{:04x}", self.subsystem_vendor_id()),
            )
            .field(
                "subsystem_id",
                &format_args!("0x{:04x}", self.subsystem_id()),
            )
            .field(
                "expansion_rom_base_address",
                &format_args!("0x{:08x}", self.expansion_rom_base_address()),
            )
            .field(
                "capabilities_pointer",
                &format_args!("0x{:02x}", self.capabilities_pointer()),
            )
            .field(
                "interrupt_line",
                &format_args!("0x{:02x}", self.interrupt_line()),
            )
            .field(
                "interrupt_pin",
                &format_args!("0x{:02x}", self.interrupt_pin()),
            )
            .field("min_grant", &format_args!("0x{:02x}", self.min_grant()))
            .field("max_latency", &format_args!("0x{:02x}", self.max_latency()))
            .finish()
    }
}

impl fmt::Debug for Header1 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(stringify!(Header1))
            .field("common", &self.common)
            .field("base_address", &self.base_address)
            .finish_non_exhaustive()
    }
}

impl fmt::Debug for Capability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(stringify!(Capability))
            .field("id", &format_args!("{:#02x}", self.id()))
            .field("next", &format_args!("{:#02x}", self.id()))
            .finish()
    }
}

impl fmt::Debug for BaseAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:08x}", self.get())
    }
}
