#![no_std]

#[macro_use]
pub mod sys;

use core::fmt;

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
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Phys(pub u64);

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

#[derive(Debug)]
#[repr(C)]
pub struct Paging {
    pub zero: [Phys; 6],
}

#[derive(Debug)]
#[repr(C)]
pub struct Pcie {
    pub base: Phys,
}

impl fmt::Debug for Phys {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x}", self.0)
    }
}
