use crate::{KernelEntryToken, page};
use core::{fmt, sync::atomic::Ordering};
use lemmings_x86_64::{
    hpet::{self, Hpet, HpetHelper},
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
    if TimeInfo::is_present() && false {
        log!("using pvclock");
        let root = unsafe { mmu::current_root::<mmu::L4>() };
        let virt = mmu::Virt::new((&raw const TIME_INFO).addr() as u64).unwrap();
        let phys = root
            .translate(&page::IdentityMapper, virt)
            .expect("TIME_INFO must be mapped");
        unsafe { TimeInfo::enable(phys) };
    } else {
        log!("calibrating with HPET");
        // FIXME get from bootinfo
        let hpet = unsafe { &*(0xfed00000 as *const Hpet) };
        let hpet = HpetHelper::new(hpet);

        // Calibrate manually
        let dt = hpet::Ns(10_000_000);
        hpet.enable();
        let end = hpet.now().saturating_add(dt);
        let t = lemmings_x86_64::tsc();
        while hpet.now() < end { /* pass */ }
        let dtsc = lemmings_x86_64::tsc() - t;
        let mut tsc_to_system_mul = (dt.0 << 32) / u128::from(dtsc);
        let mut tsc_shift = 0;
        let mut tsc_to_system_mul = loop {
            if let Ok(n) = u32::try_from(tsc_to_system_mul) {
                break n;
            } else {
                tsc_to_system_mul /= 2;
                tsc_shift += 1;
            }
        };
        while let Some(n) = tsc_to_system_mul.checked_mul(2) {
            tsc_to_system_mul = n;
            tsc_shift -= 1;
        }
        TIME_INFO
            .tsc_to_system_mul
            .store(tsc_to_system_mul, Ordering::Relaxed);
        TIME_INFO.tsc_shift.store(tsc_shift, Ordering::Relaxed);
        TIME_INFO
            .tsc_timestamp
            .store(lemmings_x86_64::tsc(), Ordering::Release);
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
