pub use lemmings_elf::Entry;

use crate::page;
use core::{num::NonZero, ops::Range, ptr::NonNull};
use lemmings_elf::PageAttr;

type LoadError = lemmings_elf::LoadError<Alloc>;

pub struct Alloc;

impl lemmings_elf::Allocator for Alloc {
    type ReserveError = i32;
    type MapError = i32;

    const PAGE_SIZE: usize = page::PAGE_SIZE;

    fn reserve(&mut self, len: NonZero<usize>) -> Result<NonNull<u8>, Self::ReserveError> {
        page::reserve_region(len)
    }

    unsafe fn alloc_region(
        &mut self,
        range: Range<NonNull<u8>>,
        attr: PageAttr,
        will_write: bool,
    ) -> Result<(), Self::MapError> {
        let attr = match (attr, will_write) {
            (x, false) => x,
            (PageAttr::R | PageAttr::RW, true) => PageAttr::RW,
            (PageAttr::RX | PageAttr::RWX, true) => PageAttr::RWX,
        };
        unsafe { page::alloc_region(range, attr) }
    }

    unsafe fn copy_to_region(&mut self, dst: NonNull<u8>, src: &[u8]) {
        unsafe { page::copy_to_region(dst, src) }
    }
}

pub fn load(elf: &[u8]) -> Result<Entry, LoadError> {
    lemmings_elf::load(elf, Alloc)
}
