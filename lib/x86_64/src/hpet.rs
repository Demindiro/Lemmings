use core::{cmp, fmt, ops};
use lemmings_volatile::VolatileCell;

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

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Monotonic<T>(pub T);
/// Nanoseconds
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Ns<T>(pub T);
/// Picoseconds
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Ps<T>(pub T);
/// Femtoseconds
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Fs<T>(pub T);

#[derive(Debug)]
pub struct HpetHelper<'a> {
    hpet: &'a Hpet,
}

#[repr(C)]
pub struct Hpet {
    capabilities_id: Reg,
    configuration: Reg,
    interrupt_status: Reg,
    _reserved: [Reg; 0xf - 0x3],
    counter: Reg,
    timers: [Timer; 32],
}

#[repr(transparent)]
pub struct CapabilitiesId(u64);

#[repr(C)]
pub struct Timer {
    configuration_capabilities: VolatileCell<u64>,
    comparator_value: VolatileCell<u64>,
    fsb_interrupt_route: VolatileCell<u64>,
    _reserved: VolatileCell<u64>,
}

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

// Dammit Intel!
#[repr(C)]
struct Reg {
    reg: VolatileCell<u64>,
    _reserved: VolatileCell<u64>,
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

    pub fn enable(&self) {
        self.hpet.configuration.set(1);
    }

    pub fn now(&self) -> Monotonic<Fs<u128>> {
        let femto_period = self.hpet.capabilities_id().period();
        Monotonic(Fs(
            u128::from(self.hpet.counter.get()) * u128::from(femto_period)
        ))
    }
}

impl Monotonic<Fs<u128>> {
    pub fn saturating_add(self, t: Ns<u128>) -> Self {
        Self(Fs(self.0.0.saturating_add(t.0.saturating_mul(1_000_000))))
    }
}

macro_rules! cmp {
    ($ty:ident) => {
        impl<T, U> cmp::PartialEq<$ty<U>> for $ty<T>
        where
            T: PartialEq<U>,
        {
            fn eq(&self, rhs: &$ty<U>) -> bool {
                self.0.eq(&rhs.0)
            }
        }

        impl<T> cmp::Eq for $ty<T> where T: Eq {}

        impl<T, U> cmp::PartialOrd<$ty<U>> for $ty<T>
        where
            T: PartialOrd<U>,
        {
            fn partial_cmp(&self, rhs: &$ty<U>) -> Option<cmp::Ordering> {
                self.0.partial_cmp(&rhs.0)
            }
        }

        impl<T> cmp::Ord for $ty<T>
        where
            T: Ord,
        {
            fn cmp(&self, rhs: &Self) -> cmp::Ordering {
                self.0.cmp(&rhs.0)
            }
        }
    };
}

cmp!(Monotonic);
cmp!(Fs);
cmp!(Ps);
cmp!(Ns);

impl ops::Deref for Reg {
    type Target = VolatileCell<u64>;

    fn deref(&self) -> &Self::Target {
        &self.reg
    }
}

impl CapabilitiesId {
    pub fn period(&self) -> u32 {
        (self.0 >> 32) as u32
    }
}
