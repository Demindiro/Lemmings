use core::{cell::RefCell, fmt};

#[derive(Clone, Copy, Default)]
struct Cap(u64);
#[derive(Clone, Copy, Default)]
pub struct PhysBase(u64);
#[derive(Clone, Copy, Default)]
pub struct PhysMask(u64);

#[derive(Clone, Copy, Debug, Default)]
pub struct Reg {
    pub base: PhysBase,
    pub mask: PhysMask,
}

pub struct Mtrr {
    cap: Cap,
}

impl Cap {
    const WC: u64 = 1 << 10;
    const FIX: u64 = 1 << 8;

    pub fn has_wc(&self) -> bool {
        self.0 & Self::WC != 0
    }

    pub fn has_fixed(&self) -> bool {
        self.0 & Self::FIX != 0
    }

    pub fn count(&self) -> u8 {
        self.0 as u8
    }
}

impl PhysBase {
    const ADDR: u64 = 0x000f_ffff_ffff_f000;

    pub fn addr(&self) -> u64 {
        self.0 & Self::ADDR
    }

    pub fn typ(&self) -> u8 {
        self.0 as u8
    }
}

impl PhysMask {
    const MASK: u64 = 0x000f_ffff_ffff_f000;
    const VALID: u64 = 1 << 11;

    pub fn mask(&self) -> u64 {
        self.0 & Self::MASK
    }

    pub fn valid(&self) -> bool {
        self.0 & Self::VALID != 0
    }
}

impl Reg {
    pub fn base(&self) -> u64 {
        self.base.addr() & self.mask.mask()
    }

    pub fn valid(&self) -> bool {
        self.mask.valid()
    }
}

impl Mtrr {
    pub fn has_wc(&self) -> bool {
        self.cap.has_wc()
    }

    pub fn iter(&self) -> impl Iterator<Item = Reg> {
        (0..self.cap.count()).filter_map(|i| self.get(i))
    }

    pub fn get(&self, index: u8) -> Option<Reg> {
        (index < self.cap.count()).then(|| unsafe {
            Reg {
                base: PhysBase(x86::msr::rdmsr(
                    x86::msr::IA32_MTRR_PHYSBASE0 + u32::from(index) * 2,
                )),
                mask: PhysMask(x86::msr::rdmsr(
                    x86::msr::IA32_MTRR_PHYSMASK0 + u32::from(index) * 2,
                )),
            }
        })
    }

    pub fn count(&self) -> u8 {
        self.cap.count()
    }
}

impl fmt::Debug for Cap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Cap")
            .field("has_wc", &self.has_wc())
            .field("has_fixed", &self.has_fixed())
            .field("count", &self.count())
            .finish()
    }
}

impl fmt::Debug for PhysBase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PhysBase")
            .field("type", &self.typ())
            .field("addr", &format_args!("{:#x}", self.addr()))
            .finish()
    }
}

impl fmt::Debug for PhysMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PhysMask")
            .field("valid", &self.valid())
            .field("mask", &format_args!("{:#x}", self.mask()))
            .finish()
    }
}

struct IterDebug<I>(RefCell<I>);

impl<I> fmt::Debug for IterDebug<I>
where
    I: Iterator,
    I::Item: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(&mut *self.0.borrow_mut()).finish()
    }
}

impl fmt::Debug for Mtrr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Mtrr")
            .field("has_wc", &self.has_wc())
            .field("fixed", &self.cap.has_fixed().then(|| ()))
            .field("registers", &IterDebug(RefCell::new(self.iter())))
            .finish()
    }
}

pub fn get() -> Option<Mtrr> {
    x86::cpuid::CpuId::new()
        .get_feature_info()?
        .has_mtrr()
        .then(|| unsafe { Cap(x86::msr::rdmsr(x86::msr::IA32_MTRRCAP)) })
        .map(|cap| Mtrr { cap })
}
