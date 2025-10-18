use core::arch::asm;

pub struct Cr3(pub u64);

pub unsafe fn current() -> Cr3 {
    let cr3;
    unsafe {
        asm!("mov {0}, cr3", out(reg) cr3);
    }
    Cr3(cr3)
}

pub unsafe fn activate(cr3: Cr3) {
    unsafe {
        asm!("mov cr3, {0}", in(reg) cr3.0);
    }
}
