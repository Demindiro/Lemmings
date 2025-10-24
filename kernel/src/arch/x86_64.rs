use core::ptr::NonNull;
use crate::{KernelEntryToken, page};
use lemmings_x86_64::{gdt::{Gdt, GdtPointer}, tss::Tss, mmu};

static mut GDT: Gdt = Gdt::new();
static mut TSS: Tss = Tss::new();

pub fn init(token: KernelEntryToken) -> KernelEntryToken {
    #[allow(static_mut_refs)]
    unsafe { GDT.set_tss(&TSS) };
    #[allow(static_mut_refs)]
    let root = unsafe { mmu::current_root::<mmu::L4>() };
    let gdtp = (&raw const GDT).addr() as u64;
    let gdtp = mmu::Virt::new(gdtp).expect("GDT should be in valid virtual space");
    let gdtp = root.translate(&page::IdentityMapper, gdtp).expect("GDT should be mapped");
    unsafe { GdtPointer::new(gdtp).activate() };
    token
}
