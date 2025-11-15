use crate::mmu;
use core::fmt;
use lemmings_volatile::VolatileCell;

const CPUID_PRESENT: u32 = 1 << 3;
const MSR_KVM_SYSTEM_TIME_NEW: u32 = 0x4b564d01;

#[repr(C, align(4))]
pub struct TimeInfo {
    // TODO volatile certainly works on x86, but is it correct?
    version: VolatileCell<u32>,
    _pad0: VolatileCell<u32>,
    tsc_timestamp: VolatileCell<u64>,
    system_time: VolatileCell<u64>,
    tsc_to_system_mul: VolatileCell<u32>,
    tsc_shift: VolatileCell<i8>,
    flags: VolatileCell<u8>,
    _pad: VolatileCell<u16>,
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
            version: VolatileCell::new(0),
            _pad0: VolatileCell::new(0),
            tsc_timestamp: VolatileCell::new(0),
            system_time: VolatileCell::new(0),
            tsc_to_system_mul: VolatileCell::new(0),
            tsc_shift: VolatileCell::new(0),
            flags: VolatileCell::new(0),
            _pad: VolatileCell::new(0),
        }
    }

    pub fn get(&self) -> TimeInfoResult {
        // https://wiki.osdev.org/Timekeeping_in_virtual_machines#pvclock
        let v = self.version.get();
        if v & 1 == 1 {
            return TimeInfoResult::Updating;
        }
        // use wrapping functions to eliminate panics
        let t = crate::tsc().wrapping_sub(self.tsc_timestamp.get());
        let s = self.tsc_shift.get();
        let t = if s < 0 {
            t.wrapping_shr(-s as _)
        } else {
            t.wrapping_shl(s as _)
        };
        let t = (u128::from(t) * u128::from(self.tsc_to_system_mul.get()) >> 32) as u64;
        let t = t.wrapping_add(self.system_time.get());
        if v != self.version.get() {
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
            .field("version", &self.version.get())
            .field("system_time", &self.system_time.get())
            .field("tsc_timestamp", &self.tsc_timestamp.get())
            .field("system_time", &self.system_time.get())
            .field("tsc_shift", &self.tsc_shift.get())
            .field("flags", &self.flags.get())
            .finish()
    }
}
