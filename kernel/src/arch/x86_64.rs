use core::ptr::NonNull;
use crate::{KernelEntryToken, page};
use lemmings_x86_64::{idt::{self, Idt, Ist, IdtPointer}, gdt::{Gdt, GdtPointer}, tss::Tss, mmu, pic};

static mut GDT: Gdt = Gdt::new();
static mut TSS: Tss = Tss::new();
static mut IDT: Idt<IDT_NR> = Idt::new();

const IDT_NR: usize = 256;

pub fn init(token: KernelEntryToken) -> KernelEntryToken {
    unsafe { pic::init() };
    let root = unsafe { mmu::current_root::<mmu::L4>() };
    init_gdt(&root);
    init_idt(&root);
    token
}

#[inline(always)]
fn init_gdt(root: &mmu::Root<mmu::L4>) {
    #[allow(static_mut_refs)]
    unsafe { GDT.set_tss(&TSS) };
    let gdtp = (&raw const GDT).addr() as u64;
    let gdtp = mmu::Virt::new(gdtp).expect("GDT should be in valid virtual space");
    let gdtp = root.translate(&page::IdentityMapper, gdtp).expect("GDT should be mapped");
    unsafe { GdtPointer::new(gdtp).activate() };
}

#[inline(always)]
fn init_idt(root: &mmu::Root<mmu::L4>) {
    let idt = unsafe { &mut *(&raw mut IDT) };

    idt.set_handler(idt::nr::DOUBLE_FAULT, double_fault as _);

    let idtp = (&raw const IDT).addr() as u64;
    let idtp = mmu::Virt::new(idtp).expect("IDT should be in valid virtual space");
    let idtp = root.translate(&page::IdentityMapper, idtp).expect("IDT should be mapped");
    unsafe { IdtPointer::new::<IDT_NR>(idtp).activate() };
}

extern "sysv64" fn double_fault() {
    panic!("Double fault!");
}
