use core::marker::PhantomData;
use vcell::VolatileCell;

mod sealed {
    pub trait Read {}
    pub trait Write {}
}

pub struct R;
pub struct W;
pub struct RW;

impl sealed::Read for R {}
impl sealed::Read for RW {}
impl sealed::Write for W {}
impl sealed::Write for RW {}

#[repr(C)]
pub struct Reg<Mode> {
    value: VolatileCell<u32>,
    _dont_touch: [VolatileCell<u32>; 3],
    _marker: PhantomData<Mode>,
}

impl<Mode: sealed::Read> Reg<Mode> {
    pub fn get(&self) -> u32 {
        self.value.get()
    }
}

impl<Mode: sealed::Write> Reg<Mode> {
    pub fn set(&self, value: u32) {
        self.value.set(value)
    }
}
