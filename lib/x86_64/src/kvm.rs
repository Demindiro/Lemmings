use crate::mmu;
use core::{
    fmt,
    sync::atomic::{AtomicI8, AtomicU8, AtomicU16, AtomicU32, AtomicU64, Ordering},
};

const CPUID_PRESENT: u32 = 1 << 3;
const MSR_KVM_SYSTEM_TIME_NEW: u32 = 0x4b564d01;

#[repr(C, align(4))]
pub struct TimeInfo {
    pub version: AtomicU32,
    _pad0: AtomicU32,
    pub tsc_timestamp: AtomicU64,
    pub system_time: AtomicU64,
    pub tsc_to_system_mul: AtomicU32,
    pub tsc_shift: AtomicI8,
    pub flags: AtomicU8,
    _pad: AtomicU16,
}

// TODO
unsafe impl Sync for TimeInfo {}

#[derive(Clone, Copy)]
pub struct Ns(pub u64);

#[derive(Debug)]
pub enum TimeInfoResult {
    Ok(Ns),
    /// The time info field is currently being updated.
    ///
    /// This is *not* an error. Simply try again later.
    Updating,
}

impl TimeInfo {
    #[inline]
    pub fn is_present() -> bool {
        unsafe { core::arch::x86_64::__cpuid(0x4000_0001).eax & CPUID_PRESENT != 0 }
    }

    /// # Safety
    ///
    /// The physical address must point to a valid [`TimeInfo`](crate::TimeInfo) structure.
    /// `MSR_KVM_SYSTEM_TIME_NEW` must be present (see [`is_present`]).
    #[inline]
    pub unsafe fn enable(phys: mmu::Phys<mmu::A2>) {
        let phys = u64::from(phys);
        unsafe {
            core::arch::asm! {
                "wrmsr",
                in("ecx") MSR_KVM_SYSTEM_TIME_NEW,
                in("edx") (phys >> 32) as u32,
                in("eax") phys as u32 | 1,
                options(nostack, preserves_flags),
            }
        }
    }

    pub const fn new() -> Self {
        Self {
            version: AtomicU32::new(0),
            _pad0: AtomicU32::new(0),
            tsc_timestamp: AtomicU64::new(0),
            system_time: AtomicU64::new(0),
            tsc_to_system_mul: AtomicU32::new(0),
            tsc_shift: AtomicI8::new(0),
            flags: AtomicU8::new(0),
            _pad: AtomicU16::new(0),
        }
    }

    pub fn get(&self) -> TimeInfoResult {
        // https://wiki.osdev.org/Timekeeping_in_virtual_machines#pvclock
        let v = self.version.load(Ordering::Acquire);
        if v & 1 == 1 {
            return TimeInfoResult::Updating;
        }
        // use wrapping functions to eliminate panics
        let t = crate::tsc().wrapping_sub(self.tsc_timestamp.load(Ordering::Relaxed));
        let s = self.tsc_shift.load(Ordering::Relaxed);
        let t = if s < 0 {
            t.wrapping_shr(-s as _)
        } else {
            t.wrapping_shl(s as _)
        };
        let t = u128::from(t) * u128::from(self.tsc_to_system_mul.load(Ordering::Relaxed)) >> 32;
        let t = (t as u64).wrapping_add(self.system_time.load(Ordering::Relaxed));
        if v != self.version.load(Ordering::Acquire) {
            return TimeInfoResult::Updating;
        }
        TimeInfoResult::Ok(Ns(t))
    }
}

impl fmt::Debug for Ns {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for Ns {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (us, ns) = (self.0 / 1000, self.0 % 1000);
        write!(f, "{us}.{ns:03}µs")
    }
}

impl fmt::Debug for TimeInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(stringify!(TimeInfo))
            .field("version", &self.version.load(Ordering::Relaxed))
            .field("system_time", &self.system_time.load(Ordering::Relaxed))
            .field("tsc_timestamp", &self.tsc_timestamp.load(Ordering::Relaxed))
            .field("system_time", &self.system_time.load(Ordering::Relaxed))
            .field("tsc_shift", &self.tsc_shift.load(Ordering::Relaxed))
            .field("flags", &self.flags.load(Ordering::Relaxed))
            .finish()
    }
}
