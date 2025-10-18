use super::{Entry, L0, L1, L2, L3, L4, PhysToPtr, Root, sealed};
use core::fmt;

pub struct Dump<'a, M, T> {
    mapper: &'a M,
    entry: &'a T,
}

fn entry_flags(e: &super::RawEntry, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{e:?} ")
}

unsafe impl sealed::Dump for Entry<L0> {
    unsafe fn dump<M>(&self, _: &M, f: &mut fmt::Formatter) -> fmt::Result
    where
        M: PhysToPtr,
    {
        if self.is_present() {
            entry_flags(&self.0, f)?;
            f.write_str("4K")
        } else {
            f.write_str("<n/a>")
        }
    }
}
unsafe impl sealed::Dump for Entry<L1> {
    unsafe fn dump<M>(&self, mapper: &M, f: &mut fmt::Formatter) -> fmt::Result
    where
        M: PhysToPtr,
    {
        if !self.is_present() {
            return f.write_str("<n/a>");
        }
        entry_flags(&self.0, f)?;
        unsafe {
            let Some(tbl) = self.table(mapper) else {
                return f.write_str("2M");
            };
            f.debug_map()
                .entries(
                    tbl.iter()
                        .enumerate()
                        .filter(|(_, e)| e.is_present())
                        .map(|(i, e)| (i, Dump::new(mapper, e))),
                )
                .finish()
        }
    }
}
unsafe impl sealed::Dump for Entry<L2> {
    unsafe fn dump<M>(&self, mapper: &M, f: &mut fmt::Formatter) -> fmt::Result
    where
        M: PhysToPtr,
    {
        if !self.is_present() {
            return f.write_str("<n/a>");
        }
        entry_flags(&self.0, f)?;
        unsafe {
            let Some(tbl) = self.table(mapper) else {
                return f.write_str("1G");
            };
            f.debug_map()
                .entries(
                    tbl.iter()
                        .enumerate()
                        .filter(|(_, e)| e.is_present())
                        .map(|(i, e)| (i, Dump::new(mapper, e))),
                )
                .finish()
        }
    }
}
unsafe impl sealed::Dump for Entry<L3> {
    unsafe fn dump<M>(&self, mapper: &M, f: &mut fmt::Formatter) -> fmt::Result
    where
        M: PhysToPtr,
    {
        if !self.is_present() {
            return f.write_str("<n/a>");
        }
        unsafe {
            let Some(tbl) = self.table(mapper) else {
                return f.write_str("512G (???)");
            };
            f.debug_map()
                .entries(
                    tbl.iter()
                        .enumerate()
                        .filter(|(_, e)| e.is_present())
                        .map(|(i, e)| (i, Dump::new(mapper, e))),
                )
                .finish()
        }
    }
}
unsafe impl sealed::Dump for Root<L4> {
    unsafe fn dump<M>(&self, mapper: &M, f: &mut fmt::Formatter) -> fmt::Result
    where
        M: PhysToPtr,
    {
        unsafe {
            f.debug_map()
                .entries(
                    self.table(mapper)
                        .iter()
                        .enumerate()
                        .filter(|(_, e)| e.is_present())
                        .map(|(i, e)| (i, Dump::new(mapper, e))),
                )
                .finish()
        }
    }
}

impl<'a, M, T> Dump<'a, M, T> {
    pub unsafe fn new(mapper: &'a M, entry: &'a T) -> Self {
        Self { mapper, entry }
    }
}

impl<'a, M, T> fmt::Debug for Dump<'a, M, T>
where
    M: PhysToPtr,
    T: sealed::Dump,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe { sealed::Dump::dump(self.entry, self.mapper, f) }
    }
}
