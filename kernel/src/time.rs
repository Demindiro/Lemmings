use crate::{KernelEntryToken, page};
use core::fmt;
use lemmings_x86_64::{
    kvm::{self, TimeInfo, TimeInfoResult},
    mmu,
};

#[derive(Default)]
pub struct Monotonic(u64);

static TIME_INFO: TimeInfo = TimeInfo::new();

impl Monotonic {
    pub fn now() -> Monotonic {
        loop {
            match TIME_INFO.get() {
                TimeInfoResult::Ok(ns) => break Self(ns.0),
                TimeInfoResult::Updating => {
                    // spinning is fine, pause hint may or may not help.
                    unsafe { core::arch::x86_64::_mm_pause() }
                }
            }
        }
    }

    pub fn nanos(&self) -> u64 {
        self.0
    }

    pub fn micros(&self) -> u64 {
        self.0 / 1000
    }
}

pub fn init(token: KernelEntryToken) -> KernelEntryToken {
    if TimeInfo::is_present() {
        let root = unsafe { mmu::current_root::<mmu::L4>() };
        let virt = mmu::Virt::new((&raw const TIME_INFO).addr() as u64).unwrap();
        let phys = root
            .translate(&page::IdentityMapper, virt)
            .expect("TIME_INFO must be mapped");
        unsafe { TimeInfo::enable(phys) };
    } else {
        todo!("manually calibrate TimeInfo with HPET");
    }
    token
}

impl fmt::Display for Monotonic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        kvm::Ns(self.0).fmt(f)
    }
}

impl fmt::Debug for Monotonic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        kvm::Ns(self.0).fmt(f)
    }
}
