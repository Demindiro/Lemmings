#![no_std]
#![deny(improper_ctypes_definitions)]

#[macro_use]
pub mod sys;

use core::{fmt, ptr::NonNull};

#[macro_export]
macro_rules! entry {
    ($func:ident) => {
        #[unsafe(no_mangle)]
        extern "sysv64" fn _start(entry: &$crate::Entry, sys: $crate::Phys, magic: u64) -> ! {
            if magic != $crate::MAGIC {
                // halt to be safe
                $crate::sys::halt();
            }
            unsafe {
                $crate::sys::ENTRY = sys.0 as _;
            }
            $func(entry);
        }
    };
}

pub const MAGIC: u64 = u64::from_le_bytes(*b"Lemmings");

pub struct Sys;

#[derive(Debug)]
#[repr(C)]
pub struct Entry {
    pub memory: MemoryMap,
    pub paging: Paging,
    pub pcie: Pcie,
    pub data: MemoryRegion,
    pub framebuffer: FrameBuffer,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Phys(pub u64);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Virt(pub NonNull<u8>);

#[derive(Debug)]
#[repr(C)]
pub struct MemoryMap {
    pub list: MemoryRegion,
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct MemoryRegion {
    /// Inclusive
    pub start: Phys,
    /// Exclusive
    pub end: Phys,
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct VirtRegion {
    /// Inclusive
    pub start: Virt,
    /// Exclusive
    pub end: Virt,
}

#[derive(Debug)]
#[repr(C)]
pub struct Paging {
    pub zero: [Phys; 3],
    pub ones: [Phys; 3],
    pub kernel: VirtRegion,
    pub stack: Virt,
}

#[derive(Debug)]
#[repr(C)]
pub struct Pcie {
    pub base: Phys,
}

#[derive(Debug)]
#[repr(C)]
pub struct FrameBuffer {
    pub base: Phys,
    pub width: u16,
    pub height: u16,
    pub stride: u16,
    pub format: ColorFormat,
}

#[derive(Debug)]
#[repr(u16)]
pub enum ColorFormat {
    None = 0,
    Rgbx8888 = 1,
    Bgrx8888 = 2,
}

impl fmt::Debug for Phys {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x}", self.0)
    }
}

impl fmt::Debug for Virt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
