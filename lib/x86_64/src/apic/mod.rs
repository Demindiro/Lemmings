pub mod io;
pub mod local;
mod reg;

use crate::mmu;
use x86::msr;

const IA32_APIC_BASE_MSR_ENABLE: u64 = 1 << 11;

pub fn local_address() -> mmu::Phys<mmu::A12> {
    let base = unsafe { msr::rdmsr(msr::IA32_APIC_BASE) };
    mmu::Phys::new_masked(base)
}

pub unsafe fn enable_local_apic(addr: mmu::Phys<mmu::A12>) {
    let v = addr.get() | IA32_APIC_BASE_MSR_ENABLE;
    unsafe { msr::wrmsr(msr::IA32_APIC_BASE, v) };
}
