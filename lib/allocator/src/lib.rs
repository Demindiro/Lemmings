#![no_std]

use core::ptr::NonNull;
use lemmings_idl_allocator as idl;

#[derive(Clone, Copy, Debug)]
pub struct Allocator {
    door: &'static idl::Allocator,
}

impl Allocator {
    pub fn get() -> Option<Self> {
        lemmings_door::door_find::<idl::Allocator>().map(|door| Self { door })
    }

    #[inline]
    pub fn alloc(self, len: usize, align: u8) -> Option<NonNull<[u8]>> {
        let x = idl::Alloc {
            len: len.into(),
            align: align.into(),
        };
        match self.door.alloc(x) {
            idl::MaybeRegion::Region(idl::Region { base, len }) => {
                Some(NonNull::slice_from_raw_parts(base.0.cast(), len.into()))
            }
            idl::MaybeRegion::NoRegion(idl::NoRegion { .. }) => None,
        }
    }

    #[inline]
    pub fn alloc_zero(self, len: usize, align: u8) -> Option<NonNull<[u8]>> {
        let ptr = self.alloc(len, align)?;
        unsafe { ptr.cast::<u8>().write_bytes(0, ptr.len()) };
        Some(ptr)
    }

    #[inline]
    pub unsafe fn free(self, base: NonNull<u8>, len: usize) {
        let x = idl::Free {
            base: base.cast().into(),
            len: len.into(),
        };
        self.door.free(x)
    }
}
