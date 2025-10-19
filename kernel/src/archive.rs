use crate::{KernelEntryToken, page};

static mut BASE: &'static [u8] = &[];

pub fn init(entry: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    unsafe {
        let start = page::phys_to_virt(entry.data.start).cast::<u8>();
        let end = page::phys_to_virt(entry.data.end).cast::<u8>();
        let len = end.offset_from(start) as usize;
        BASE = core::slice::from_raw_parts(start.as_ptr(), len);
    }
    token
}
