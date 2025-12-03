pub use lemmings_elf::Entry;

use crate::page;
use core::{num::NonZero, ops::Range, ptr::NonNull};
use lemmings_elf::PageAttr;

pub mod door {
    use crate::page;
    use core::slice;
    use lemmings_idl_loader_elf::*;

    door! {
        [lemmings_idl_loader_elf Loader "ELF loader"]
        load
    }

    unsafe fn load(x: Load) -> LoadResult {
        let Load {
            elf_base,
            elf_len,
            reason_base,
            reason_len,
        } = x;
        let elf =
            unsafe { slice::from_raw_parts(elf_base.0.as_ptr().cast::<u8>(), elf_len.into()) };
        let reason = unsafe {
            slice::from_raw_parts_mut(
                reason_base.0.as_ptr().cast::<u8>(),
                u16::from(reason_len).into(),
            )
        };
        match super::load(elf) {
            Ok(entry) => LoadOk {
                entry: unsafe { core::mem::transmute(entry) },
                reason_len: 0.into(),
            }
            .into(),
            Err(err) => {
                use super::LoadError as E;
                // TODO write reason string.
                let reason_len = 0;
                let _ = reason;
                LoadFail {
                    reason: match err {
                        E::Not64Bit
                        | E::NotLittleEndian
                        | E::NotRelocatable
                        | E::UnsupportedArchitecture
                        | E::UnsupportedVersion
                        | E::UnsupportedSegmentFlags => Reason::Unsupported,
                        E::BadMagic
                        | E::EntryOutOfBounds
                        | E::ProgramHeadersTruncated
                        | E::SegmentNotAligned
                        | E::TruncatedHeader
                        | E::UnexpectedProgramHeaderSize
                        | E::VirtualSizeZero => Reason::Malformed,
                        E::ReserveRegion(x) => todo!("concrete error {x}"),
                        E::MapRegion(x) => todo!("concrete error {x}"),
                    },
                    reason_len: reason_len.into(),
                }
                .into()
            }
        }
    }
}

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
