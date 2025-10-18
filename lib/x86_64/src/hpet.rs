use core::fmt;

use vcell::VolatileCell;

pub trait LossyConvert<To> {
    fn convert(self) -> To;
}

pub trait Div1e3 {
    fn div_1e3(self) -> Self;
}

impl Div1e3 for u128 {
    fn div_1e3(self) -> Self {
        self / 1000
    }
}

#[repr(transparent)]
pub struct Monotonic<T>(pub T);
/// Nanoseconds
#[repr(transparent)]
pub struct Ns<T>(pub T);
/// Picoseconds
#[repr(transparent)]
pub struct Ps<T>(pub T);
/// Femtoseconds
#[repr(transparent)]
pub struct Fs<T>(pub T);

impl<T> LossyConvert<Ps<T>> for Fs<T>
where
    T: Div1e3,
{
    fn convert(self) -> Ps<T> {
        Ps(self.0.div_1e3())
    }
}

impl<T> LossyConvert<Ns<T>> for Ps<T>
where
    T: Div1e3,
{
    fn convert(self) -> Ns<T> {
        Ns(self.0.div_1e3())
    }
}

impl<T> LossyConvert<Ns<T>> for Fs<T>
where
    T: Div1e3,
{
    fn convert(self) -> Ns<T> {
        let Ps(ps) = self.convert();
        Ps(ps).convert()
    }
}

pub struct HpetHelper<'a> {
    hpet: &'a Hpet,
}

#[repr(C)]
pub struct Hpet {
    capabilities_id: VolatileCell<u64>,
    configuration: VolatileCell<u64>,
    interrupt_status: VolatileCell<u64>,
    _reserved: [VolatileCell<u64>; 0xc],
    pub counter: VolatileCell<u64>,
}

impl Hpet {
    fn capabilities_id(&self) -> CapabilitiesId {
        CapabilitiesId(self.capabilities_id.get())
    }
}

impl fmt::Debug for Hpet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut f = f.debug_struct(stringify!(Hpet));
        let cap = self.capabilities_id.get();
        f.field("period", &(cap >> 32));
        f.field("vendor_id", &format_args!("{:#x}", (cap >> 16) as u16));
        f.field("capabilities", &format_args!("{:#x}", cap & 0xffff_ffff));
        f.field(
            "configuration",
            &format_args!("{:#x}", self.configuration.get()),
        );
        f.field(
            "interrupt_status",
            &format_args!("{:#x}", self.interrupt_status.get()),
        );
        f.field("counter", &self.counter.get());
        f.finish()
    }
}

impl<'a> HpetHelper<'a> {
    pub fn new(hpet: &'a Hpet) -> Self {
        Self { hpet }
    }

    pub unsafe fn enable(&self) {
        self.hpet
            .configuration
            .set(self.hpet.configuration.get() | 1);
    }

    pub fn now(&self) -> Monotonic<Fs<u128>> {
        let femto_period = self.hpet.capabilities_id().period();
        Monotonic(Fs(
            u128::from(self.hpet.counter.get()) * u128::from(femto_period)
        ))
    }
}

#[repr(transparent)]
pub struct CapabilitiesId(u64);

impl CapabilitiesId {
    pub fn period(&self) -> u32 {
        (self.0 >> 32) as u32
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct Timer {
    configuration_capabilities: VolatileCell<u64>,
    comparator_value: VolatileCell<u64>,
    fsb_interrupt_route: VolatileCell<u64>,
}
