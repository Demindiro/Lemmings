use crate::syscall;
use core::{
    num::NonZero,
    ops::Range,
    ptr::{self, NonNull},
};
use lemmings_elf::PageAttr;

pub const PAGE_SIZE: usize = 1 << 12;

pub fn reserve_region(len: NonZero<usize>) -> Result<NonNull<u8>, i32> {
    unsafe { memmap(None, None, len.get()) }
}

pub unsafe fn alloc_region(va: Range<NonNull<u8>>, attr: PageAttr) -> Result<(), i32> {
    let len = unsafe { va.end.offset_from(va.start) as usize };
    unsafe { memmap(Some(va.start), Some(attr), len).map(|_| ()) }
}

pub unsafe fn copy_to_region(dst: NonNull<u8>, src: &[u8]) {
    unsafe { dst.copy_from_nonoverlapping(NonNull::from(src).cast(), src.len()) }
}

unsafe fn memmap(
    ptr: Option<NonNull<u8>>,
    attr: Option<PageAttr>,
    len: usize,
) -> Result<NonNull<u8>, i32> {
    let mut flags = syscall::MAP_ANON | syscall::MAP_PRIVATE;
    flags |= u32::from(ptr.is_some()) * syscall::MAP_FIXED;
    let prot = match attr {
        None => syscall::PROT_NONE,
        Some(PageAttr::R) => syscall::PROT_R,
        Some(PageAttr::RW) => syscall::PROT_RW,
        Some(PageAttr::RX) => syscall::PROT_RX,
        Some(PageAttr::RWX) => syscall::PROT_RWX,
    };
    let ptr = unsafe {
        syscall::mmap(
            ptr.map_or_else(ptr::null_mut, |x| x.as_ptr()),
            len,
            prot,
            flags,
            0,
            0,
        )
    };
    match ptr as isize {
        x @ -4095..0 => Err(x as i32),
        _ => NonNull::new(ptr).ok_or(0),
    }
}
