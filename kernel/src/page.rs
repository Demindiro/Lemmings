use core::ptr::NonNull;

pub const PAGE_SIZE: usize = 4096;

pub struct OutOfMemory;

pub fn alloc_one() -> Result<NonNull<u8>, OutOfMemory> {
    return Ok(NonNull::new(0x9000 as _).unwrap());
    Err(OutOfMemory)
}
