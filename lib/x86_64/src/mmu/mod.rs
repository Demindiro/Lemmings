mod cr3;
mod dump;

use bitflags::bitflags;
pub use dump::Dump;

use core::{arch::asm, fmt, marker::PhantomData, ptr::NonNull};

mod sealed {
    pub trait Level: Sized {
        fn is_page(entry: &super::Entry<Self>) -> bool;
    }
    pub trait Page: Level {
        fn set_attr(entry: &mut super::Entry<Self>, attr: super::PageAttr);
    }
    pub trait Table: Level {
        type Lower;
    }
    pub trait Root {}
    pub trait Align {
        const BITS: u8;
        const ALIGN: u64 = 1 << Self::BITS;
        const MASK: u64 = Self::ALIGN - 1;
    }
    pub trait AlignedTo<A> {}
    pub unsafe trait Dump {
        unsafe fn dump<M>(&self, mapper: &M, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result
        where
            M: super::PhysToPtr;
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub struct PageAttr: u8 {
        const R = 1 << 0;
        const W = 1 << 1;
        const X = 1 << 2;
        const RW = Self::R.bits() | Self::W.bits();
        const RX = Self::R.bits() | Self::X.bits();
        const RWX = Self::RW.bits() | Self::X.bits();
    }
}

pub trait PhysToVirt {
    fn virt<A>(&self, phys: Phys<A>) -> Virt<A>;
}

pub unsafe trait PhysToPtr: PhysToVirt {
    fn ptr<A>(&self, phys: Phys<A>) -> NonNull<u8> {
        unsafe { NonNull::new_unchecked(self.virt(phys).as_ptr()) }
    }
}

pub unsafe trait PageAllocator {
    type Error;

    fn alloc(&mut self) -> Result<(NonNull<u8>, Phys<A12>), Self::Error>;

    fn alloc_zeroed(&mut self) -> Result<(NonNull<u8>, Phys<A12>), Self::Error> {
        let (ptr, phys) = self.alloc()?;
        unsafe { ptr.write_bytes(0, PAGE_SIZE as usize) };
        Ok((ptr, phys))
    }
}

pub struct L0;
pub struct L1;
pub struct L2;
pub struct L3;
pub struct L4;
pub struct L5;

pub struct PhysSpace;
pub struct VirtSpace;

#[repr(transparent)]
pub struct Addr<A, S>(u64, PhantomData<(A, S)>);
pub type Phys<A> = Addr<A, PhysSpace>;
pub type Virt<A> = Addr<A, VirtSpace>;

pub const PAGE_SIZE: u64 = 1 << 12;
pub const PAGE_MASK: u64 = PAGE_SIZE - 1;
pub const TABLE_SIZE: u64 = PAGE_SIZE / 8;

// Don't implement copy to avoid accidentally updating stack values instead of
// entries in a table.
#[derive(Clone)]
struct RawEntry(u64);

#[derive(Clone)]
struct RawRoot(u64);

#[repr(transparent)]
pub struct Entry<Level>(RawEntry, PhantomData<Level>);

#[repr(transparent)]
pub struct Root<Level>(RawRoot, PhantomData<Level>);

pub type Page = Entry<L0>;
pub type Pt = Entry<L1>;
pub type Pd = Entry<L2>;
pub type Pdp = Entry<L3>;
pub type Pml4 = Entry<L4>;
pub type Pml5 = Entry<L5>;

type Table<T> = [T; TABLE_SIZE as usize];

pub type PtTable = Table<Pt>;
pub type PdTable = Table<Pd>;
pub type PdpTable = Table<Pdp>;
pub type Pml4Table = Table<Pml4>;
pub type Pml5Table = Table<Pml5>;

pub type Page4K = Entry<L0>;
pub type Page2M = Entry<L1>;
pub type Page1G = Entry<L2>;

impl<A: sealed::Align, S> Addr<A, S> {
    pub const fn new(addr: u64) -> Option<Self> {
        if addr & A::MASK == 0 {
            Some(Self(addr, PhantomData))
        } else {
            None
        }
    }

    pub const fn new_masked(addr: u64) -> Self {
        Self(addr & !A::MASK, PhantomData)
    }

    /// # Safety
    ///
    /// The address must be properly aligned.
    pub const unsafe fn new_unchecked(addr: u64) -> Self {
        debug_assert!(addr & A::MASK == 0, "address not aligned");
        Self(addr, PhantomData)
    }

    pub const fn step_next(self, count: u64) -> Self {
        Self(self.0 + A::ALIGN * count, self.1)
    }

    pub const fn step_prev(self, count: u64) -> Self {
        Self(self.0 - A::ALIGN * count, self.1)
    }
}

impl<A, S> Addr<A, S> {
    pub const fn cast<T>(self) -> Addr<A, T> {
        Addr(self.0, PhantomData)
    }

    pub const fn get(&self) -> u64 {
        self.0
    }
}

impl<A> Virt<A> {
    pub fn as_ptr<T>(self) -> *mut T {
        self.0 as _
    }
}

impl<A, S> Clone for Addr<A, S> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<A, S> Copy for Addr<A, S> {}

impl<AA, AB, S> PartialEq<Addr<AA, S>> for Addr<AB, S> {
    fn eq(&self, rhs: &Addr<AA, S>) -> bool {
        self.0 == rhs.0
    }
}

impl<A, S> Eq for Addr<A, S> {}

impl<A, S> From<Addr<A, S>> for u64 {
    fn from(addr: Addr<A, S>) -> u64 {
        addr.0
    }
}

impl sealed::Level for L0 {
    fn is_page(_: &Entry<L0>) -> bool {
        true
    }
}
impl sealed::Level for L1 {
    fn is_page(entry: &Entry<L1>) -> bool {
        entry.0.is_page()
    }
}
impl sealed::Level for L2 {
    fn is_page(entry: &Entry<L2>) -> bool {
        entry.0.is_page()
    }
}
impl sealed::Level for L3 {
    fn is_page(entry: &Entry<L3>) -> bool {
        entry.0.is_page()
    }
}
impl sealed::Level for L4 {
    fn is_page(_: &Entry<L4>) -> bool {
        false
    }
}
impl sealed::Level for L5 {
    fn is_page(_: &Entry<L5>) -> bool {
        false
    }
}

impl sealed::Table for L1 {
    type Lower = L0;
}
impl sealed::Table for L2 {
    type Lower = L1;
}
impl sealed::Table for L3 {
    type Lower = L2;
}
impl sealed::Table for L4 {
    type Lower = L3;
}
impl sealed::Table for L5 {
    type Lower = L4;
}

impl sealed::Page for L0 {
    fn set_attr(entry: &mut Entry<L0>, attr: PageAttr) {
        entry.0.set_attr_4k(attr)
    }
}
impl sealed::Page for L1 {
    fn set_attr(entry: &mut Entry<L1>, attr: PageAttr) {
        entry.0.set_attr_2m(attr)
    }
}
impl sealed::Page for L2 {
    fn set_attr(entry: &mut Entry<L2>, attr: PageAttr) {
        entry.0.set_attr_2m(attr)
    }
}

impl sealed::Root for L4 {}
impl sealed::Root for L5 {}

macro_rules! align {
    ($n:literal $a:ident, $($ns:literal $as:ident,)*) => {
        pub struct $a;
        impl sealed::Align for $a {
            const BITS: u8 = $n;
        }
        impl sealed::AlignedTo<$a> for $a {}
        $(impl sealed::AlignedTo<$a> for $as {})*
        $(impl<S> From<Addr<$as, S>> for Addr<$a, S> {
            fn from(x: Addr<$as, S>) -> Self {
                Addr(x.0, PhantomData::<($a, S)>)
            }
        })*
        $(const _: () = { use sealed::Align; assert!((<$a>::BITS) < (<$as>::BITS)) };)*
        align!($($ns $as,)*);
    };
    () => {};
}
align! {
    0 A0,
    1 A1,
    2 A2,
    3 A3,
    4 A4,
    5 A5,
    6 A6,
    12 A12,
    14 A14,
    21 A21,
    30 A30,
}

impl<Level> Entry<Level>
where
    Level: sealed::Level,
{
    pub fn is_present(&self) -> bool {
        self.0.is_present()
    }

    pub fn phys(&self) -> Phys<A12> {
        self.0.phys()
    }

    pub fn is_page(&self) -> bool {
        Level::is_page(self)
    }
}

impl<Level> Entry<Level>
where
    Level: sealed::Table + sealed::Page,
{
    pub fn is_table(&self) -> bool {
        self.0.is_table()
    }
}

impl<Level> Entry<Level>
where
    Level: sealed::Page,
{
    fn replace<A: sealed::AlignedTo<A12>>(&mut self, phys: Phys<A>) -> Option<Phys<A12>> {
        let prev = self.is_present().then(|| self.phys());
        self.0.set_phys(phys);
        self.0.0 |= RawEntry::PRESENT;
        prev
    }

    pub fn set_attr(&mut self, attr: PageAttr) {
        Level::set_attr(self, attr)
    }
}

impl<Level> Entry<Level>
where
    Level: sealed::Table,
{
    pub unsafe fn table_ptr<M>(&self, mapper: &M) -> Option<NonNull<Table<Entry<Level::Lower>>>>
    where
        M: PhysToPtr,
    {
        (self.is_present() && !self.is_page()).then(|| {
            let phys = self.0.phys();
            mapper.ptr(phys).cast()
        })
    }

    pub unsafe fn table<M>(&self, mapper: &M) -> Option<&'static Table<Entry<Level::Lower>>>
    where
        M: PhysToPtr,
    {
        unsafe { self.table_ptr(mapper).map(|p| &*p.as_ptr()) }
    }

    pub unsafe fn table_mut<M>(&self, mapper: &M) -> Option<&'static mut Table<Entry<Level::Lower>>>
    where
        M: PhysToPtr,
    {
        unsafe { self.table_ptr(mapper).map(|p| &mut *p.as_ptr()) }
    }

    pub unsafe fn table_mut_or_insert<M, A>(
        &mut self,
        mapper: &M,
        alloc: &mut A,
    ) -> Result<&'static mut Table<Entry<Level::Lower>>, A::Error>
    where
        M: PhysToPtr,
        A: PageAllocator + ?Sized,
    {
        if let Some(tbl) = unsafe { self.table_mut(mapper) } {
            return Ok(tbl);
        }
        let (ptr, phys) = alloc.alloc_zeroed()?;
        self.0 = RawEntry::new_table(phys);
        Ok(unsafe { &mut *ptr.as_ptr().cast() })
    }
}

impl<Level> Root<Level> {
    pub fn from_phys(phys: Phys<A12>) -> Self {
        Self(RawRoot::new(phys), PhantomData)
    }
}

impl<Level> Root<Level>
where
    Level: sealed::Root + sealed::Table,
{
    pub unsafe fn table_ptr<M>(&self, mapper: &M) -> NonNull<Table<Entry<Level::Lower>>>
    where
        M: PhysToPtr,
    {
        let phys = self.0.phys();
        mapper.ptr(phys).cast()
    }

    pub unsafe fn table<M>(&self, mapper: &M) -> &'static Table<Entry<Level::Lower>>
    where
        M: PhysToPtr,
    {
        unsafe { self.table_ptr(mapper).as_ref() }
    }

    pub unsafe fn table_mut<M>(&self, mapper: &M) -> &'static mut Table<Entry<Level::Lower>>
    where
        M: PhysToPtr,
    {
        unsafe { self.table_ptr(mapper).as_mut() }
    }
}

// TODO generalize to PML5 - will require trait magic
impl Root<L4> {
    // TODO reuse code better...
    pub unsafe fn set_1g<M, A>(
        &mut self,
        mapper: &M,
        alloc: &mut A,
        virt: Virt<A30>,
        phys: Phys<A30>,
        attr: PageAttr,
    ) -> Result<Option<Phys<A12>>, A::Error>
    where
        M: PhysToPtr,
        A: PageAllocator + ?Sized,
    {
        let ([_, _, l2, l3], _) = Self::indices(virt);
        unsafe {
            let entry = &mut self.table_mut(mapper)[l3];
            let entry = &mut entry.table_mut_or_insert(mapper, alloc)?[l2];
            entry.set_attr(attr);
            Ok(entry.replace(phys))
        }
    }

    pub unsafe fn set_2m<M, A>(
        &mut self,
        mapper: &M,
        alloc: &mut A,
        virt: Virt<A21>,
        phys: Phys<A21>,
        attr: PageAttr,
    ) -> Result<Option<Phys<A12>>, A::Error>
    where
        M: PhysToPtr,
        A: PageAllocator + ?Sized,
    {
        let ([_, l1, l2, l3], _) = Self::indices(virt);
        unsafe {
            let entry = &mut self.table_mut(mapper)[l3];
            let entry = &mut entry.table_mut_or_insert(mapper, alloc)?[l2];
            let entry = &mut entry.table_mut_or_insert(mapper, alloc)?[l1];
            entry.set_attr(attr);
            Ok(entry.replace(phys))
        }
    }

    pub unsafe fn set_4k<M, A>(
        &mut self,
        mapper: &M,
        alloc: &mut A,
        virt: Virt<A12>,
        phys: Phys<A12>,
        attr: PageAttr,
    ) -> Result<Option<Phys<A12>>, A::Error>
    where
        M: PhysToPtr,
        A: PageAllocator + ?Sized,
    {
        let ([l0, l1, l2, l3], _) = Self::indices(virt);
        unsafe {
            let entry = &mut self.table_mut(mapper)[l3];
            let entry = &mut entry.table_mut_or_insert(mapper, alloc)?[l2];
            let entry = &mut entry.table_mut_or_insert(mapper, alloc)?[l1];
            let entry = &mut entry.table_mut_or_insert(mapper, alloc)?[l0];
            entry.set_attr(attr);
            Ok(entry.replace(phys))
        }
    }

    pub unsafe fn copy_from_slice<M>(
        &mut self,
        mapper: &M,
        virt: Virt<A0>,
        bytes: &[u8],
    ) -> Result<(), ()>
    where
        M: PhysToPtr,
    {
        let ([l0, l1, l2, l3], offset) = Self::indices(virt);
        assert!(
            bytes.len() <= PAGE_SIZE as usize - offset,
            "todo: multi-page"
        );
        unsafe {
            let entry = &self.table(mapper)[l3];
            let entry = &entry.table(mapper).ok_or(())?[l2];
            let entry = &entry.table(mapper).ok_or(())?[l1];
            let entry = &entry.table(mapper).ok_or(())?[l0];
            mapper
                .ptr(entry.phys())
                .cast::<u8>()
                .as_ptr()
                .add(offset)
                .copy_from_nonoverlapping(bytes.as_ptr(), bytes.len());
        }
        Ok(())
    }

    /// Translate a single virtual address to a physical address.
    pub fn translate<M, A>(&self, mapper: &M, addr: Virt<A>) -> Option<Phys<A>>
    where
        A12: sealed::AlignedTo<A>,
        M: PhysToPtr,
    {
        let ([l0, l1, l2, l3], _) = Self::indices(addr);
        let f = || unsafe {
            let entry = &self.table(mapper)[l3];
            assert!(!entry.is_page());
            let entry = &entry.table(mapper)?[l2];
            if entry.is_page() {
                return Some((entry.phys(), 30));
            }
            let entry = &entry.table(mapper)?[l1];
            if entry.is_page() {
                return Some((entry.phys(), 21));
            }
            let entry = &entry.table(mapper)?[l0];
            if entry.is_page() {
                return Some((entry.phys(), 12));
            }
            None
        };
        let (base, maskbits) = f()?;
        let mask = (1 << maskbits) - 1;
        Some(Addr((base.0 & !mask) | (addr.0 & mask), PhantomData))
    }

    fn indices<A>(virt: Virt<A>) -> ([usize; 4], usize) {
        let (virt, offt) = (virt.0 / PAGE_SIZE, virt.0 % PAGE_SIZE);
        let (virt, l0) = (virt / TABLE_SIZE, virt % TABLE_SIZE);
        let (virt, l1) = (virt / TABLE_SIZE, virt % TABLE_SIZE);
        let (virt, l2) = (virt / TABLE_SIZE, virt % TABLE_SIZE);
        let (virt, l3) = (virt / TABLE_SIZE, virt % TABLE_SIZE);
        // TODO we should use *signed* integers, will make things simpler :)
        debug_assert!(
            virt == 0 || virt == u64::MAX / (PAGE_SIZE * TABLE_SIZE.pow(4)),
            "virtual address not in valid half"
        );
        ([l0, l1, l2, l3].map(|x| x as usize), offt as usize)
    }
}

/// ## IA32_PAT setup
///
/// ```
///  7  6  5  4  3  2  1  0
/// UC UC WC WB UC UC WC WB
/// ```
impl RawEntry {
    const PRESENT: u64 = 1 << 0;
    const READ_WRITE: u64 = 1 << 1;
    const USER: u64 = 1 << 2;
    #[allow(dead_code)]
    const WRITE_THROUGH: u64 = 1 << 3;
    #[allow(dead_code)]
    const CACHE_DISABLE: u64 = 1 << 4;
    #[allow(dead_code)]
    const ACCESSED: u64 = 1 << 5;
    const PAGE_SIZE: u64 = 1 << 7;
    const GLOBAL: u64 = 1 << 8;
    #[allow(dead_code)]
    const AVAILABLE: u64 = 7 << 9;
    const EXECUTE_DISABLE: u64 = 1 << 63;

    #[allow(dead_code)]
    const PAT_4K: u64 = 1 << 7;
    #[allow(dead_code)]
    const PAT_2M: u64 = 1 << 12;

    /// Clears lower *and* upper bits
    const PHYS_MASK: u64 = ((1 << 52) - 1) & !PAGE_MASK;

    pub fn new_table(phys: Phys<A12>) -> Self {
        Self(phys.0 | Self::PRESENT | Self::READ_WRITE)
    }

    pub fn is_present(&self) -> bool {
        self.0 & Self::PRESENT > 0
    }

    pub fn is_table(&self) -> bool {
        self.is_present() && !self.is_page()
    }

    pub fn is_page(&self) -> bool {
        self.is_present() && self.0 & Self::PAGE_SIZE > 0
    }

    pub fn is_user(&self) -> bool {
        self.is_present() && self.0 & Self::USER > 0
    }

    pub fn is_writeable(&self) -> bool {
        self.is_present() && self.0 & Self::READ_WRITE > 0
    }

    pub fn is_global(&self) -> bool {
        self.is_present() && self.0 & Self::GLOBAL > 0
    }

    pub fn phys(&self) -> Phys<A12> {
        Phys::new_masked(self.0 & Self::PHYS_MASK)
    }

    pub fn set_phys<A: sealed::AlignedTo<A12>>(&mut self, phys: Phys<A>) {
        self.0 |= phys.0;
    }

    pub fn set_attr_4k(&mut self, attr: PageAttr) {
        self.0 &= !Self::READ_WRITE;
        self.set_attr(attr);
    }

    pub fn set_attr_2m(&mut self, attr: PageAttr) {
        self.0 &= !Self::READ_WRITE;
        self.0 |= Self::PAGE_SIZE;
        self.set_attr(attr);
    }

    fn set_attr(&mut self, attr: PageAttr) {
        if attr.contains(PageAttr::W) {
            self.0 |= Self::READ_WRITE;
        }
        if !attr.contains(PageAttr::X) {
            self.0 |= Self::EXECUTE_DISABLE;
        }
    }
}

impl RawRoot {
    pub fn new(phys: Phys<A12>) -> Self {
        Self(phys.0)
    }

    pub fn phys(&self) -> Phys<A12> {
        Phys::new_masked(self.0)
    }
}

impl<Level> Clone for Entry<Level> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1)
    }
}

impl<Level> Clone for Root<Level> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1)
    }
}

impl<A, S> fmt::Debug for Addr<A, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x}", self.0)
    }
}

impl fmt::Debug for RawEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "0x{:x}:{}{}{}{}{}",
            self.0 & !0xfff,
            if self.is_present() { "P" } else { "-" },
            self.is_page().then(|| "L").unwrap_or("-"),
            self.is_writeable().then(|| "W").unwrap_or("-"),
            self.is_user().then(|| "U").unwrap_or("-"),
            self.is_global().then(|| "G").unwrap_or("-"),
        )
    }
}

pub unsafe fn invalidate_page(address: NonNull<Page>) {
    // SAFETY: invlpg is always safe to use (if in kernel mode).
    unsafe {
        asm!("invlpg [{}]", in(reg) address.as_ptr(), options(nostack, preserves_flags));
    }
}

pub unsafe fn set_root<Level>(root: &Root<Level>)
where
    Level: sealed::Root,
{
    unsafe { cr3::activate(cr3::Cr3(root.0.0)) }
}

pub unsafe fn current_root<Level>() -> Root<Level>
where
    Level: sealed::Root,
{
    unsafe { Root(RawRoot(cr3::current().0), PhantomData) }
}

pub fn support_1gb() -> bool {
    x86::cpuid::CpuId::new()
        .get_extended_processor_and_feature_identifiers()
        .is_some_and(|x| x.has_1gib_pages())
}
