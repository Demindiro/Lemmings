use crate::linux;

#[derive(Default)]
pub struct Monotonic(u64);

static mut START: u64 = 0;

impl Monotonic {
    pub fn now() -> Self {
        let t = linux::clock_gettime(linux::CLOCK_MONOTONIC).expect("CLOCK_MONOTONIC");
        let t = t.secs * 1_000_000_000 + u64::from(t.nsecs);
        Self(t - unsafe { START })
    }

    pub fn nanos(&self) -> u64 {
        self.0
    }

    pub fn micros(&self) -> u64 {
        self.0 / 1000
    }
}

pub unsafe fn init() {
    let t = Monotonic::now();
    unsafe { START = t.0 };
}
