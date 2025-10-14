#![no_std]

use core::{cell::UnsafeCell, ptr::NonNull};

const KERNEL_FILENAME: &str = "opt/lemmings/kernel.elf";

mod x86 {
    pub unsafe fn out8(port: u16, value: u8) {
        unsafe {
            core::arch::asm! {
                "out dx, eax",
                in("dx") port,
                in("al") value,
            }
        }
    }
    pub unsafe fn out16(port: u16, value: u16) {
        unsafe {
            core::arch::asm! {
                "out dx, eax",
                in("dx") port,
                in("ax") value,
            }
        }
    }
    pub unsafe fn out32(port: u16, value: u32) {
        unsafe {
            core::arch::asm! {
                "out dx, eax",
                in("dx") port,
                in("eax") value,
            }
        }
    }
}

mod sys {
    use crate::*;
    use core::{arch::asm, ptr::{self, NonNull}};

    pub struct OpenFileError;

    pub fn print(s: &str) {
        unsafe {
            asm! {
                "call sys_print",
                in("rcx") s.len(),
                in("rsi") s.as_ptr(),
                lateout("rcx") _,
                lateout("rdx") _,
                lateout("rsi") _,
            }
        }
    }

    fn exit(status: i32) -> ! {
        unsafe {
            asm! {
                "jmp sys_exit",
                in("edx") status,
                options(noreturn),
            }
        }
    }

    pub fn exit_err() -> ! {
        exit(1)
    }

    pub fn open(filename: &str) -> Result<usize, OpenFileError> {
        let len: isize;
        unsafe {
            asm! {
                "call sys_open",
                inout("rcx") filename.len() => _,
                inout("rsi") filename.as_ptr() => _,
                lateout("rdi") _,
                lateout("rdx") _,
                lateout("rax") len,
            }
        }
        usize::try_from(len).map_err(|_| OpenFileError)
    }

    pub fn read(buf: &mut [u8]) {
        unsafe {
            asm! {
                "call sys_read",
                inout("rcx") buf.len() => _,
                inout("rdi") buf.as_ptr() => _,
                out("rdx") _,
                out("rax") _,
            }
        }
    }

    /// # Safety
    ///
    /// Only one (mutable!) reference to the PML4 may exist at any time.
    ///
    /// Care must be taken when updating live entries.
    pub unsafe fn pml4() -> page::Pml4 {
        unsafe {
            let addr: usize;
            asm! {
                "mov rax, cr3",
                out("rax") addr,
            }
            let addr = ptr::with_exposed_provenance_mut(addr);
            let addr = NonNull::new_unchecked(addr);
            page::Pml4::from_ptr(addr)
        }
    }

    /// # Safety
    ///
    /// The page mappings must be valid.
    ///
    /// In particular: the page with the current IP must be properly mapped.
    pub unsafe fn set_pml4(pml4: page::Pml4) {
        unsafe {
            asm! {
                "mov cr3, rax",
                in("rax") pml4.as_u64(),
            }
        }
    }
}

mod alloc {
    use crate::*;
    use core::ops::Range;

    pub const PAGE_SIZE: usize = 4096;
    const MAX_REGIONS: usize = 8;

    pub struct Allocator {
        regions: [Range<u64>; MAX_REGIONS],
    }

    fn round_p2(x: usize, n: usize) -> usize {
        let mask = n - 1;
        (x + mask) & !mask
    }

    impl Allocator {
        pub fn new() -> Self {
            let mut regions: [_; MAX_REGIONS] = Default::default();
            // intentionally skip 0x0..0x3000 because:
            // - we use 0x0..0x1000 as stack
            // - we use 0x1000..0x2000 as PML4
            // - we use 0x2000..0x3000 as PDPT
            // - we use 0x3000..0x4000 and 0x4000..0x5000 as PD
            // - we use 0x5000..0x6000 as PT
            // - we can't safely dereference 0x0 in Rust
            regions[0] = 0x6000..0xb0000;
            Self { regions }
        }

        pub fn alloc(&mut self, byte_count: usize) -> Option<NonNull<u8>> {
            let n = round_p2(byte_count, PAGE_SIZE) as u64;
            for r in &mut self.regions {
                if r.end - r.start >= n {
                    let s = r.start;
                    r.start += n;
                    return NonNull::new(s as *mut u8);
                }
            }
            None
        }
    }
}

mod page {
    use crate::*;
    use core::{ptr::{self, NonNull}, ops};

    macro_rules! impl_p {
        (T $table:ident $entry:ident) => {
            pub struct $table(NonNull<[$entry; 512]>);
            pub struct $entry(u64);

            #[allow(dead_code)]
            impl $table {
                pub fn new(alloc: &mut alloc::Allocator) -> Option<Self> {
                    alloc.alloc(alloc::PAGE_SIZE).map(NonNull::cast).map(Self)
                }

                pub fn as_u64(&self) -> u64 {
                    //self.0.addr().get() as u64
                    //self.0.expose_provenance().get() as u64
                    self.0.as_ptr() as u64
                }

                pub unsafe fn from_ptr(ptr: NonNull<[$entry; 512]>) -> Self {
                    Self(ptr)
                }
            }

            impl ops::Deref for $table {
                type Target = [$entry; 512];

                fn deref(&self) -> &Self::Target {
                    unsafe { &*self.0.as_ptr() }
                }
            }

            impl ops::DerefMut for $table {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    unsafe { &mut *self.0.as_ptr() }
                }
            }
        };
        (E $entry:ident $table:ident $mask:literal) => {
            #[allow(dead_code)]
            impl $entry {
                pub fn get_table(&self) -> Option<$table> {
                    self.is_table().then(|| $table(unsafe { NonNull::new_unchecked(self.addr_table_ptr()) }))
                }

                /// # Note
                ///
                /// Only inserts a table if not present
                pub fn get_or_alloc_table(&mut self, alloc: &mut alloc::Allocator) -> Option<$table> {
                    self.get_table().or_else(|| (!self.is_present()).then(|| {
                        let Some(table) = $table::new(alloc) else { fail("out of memory") };
                        self.0 = table.as_u64() | PRESENT;
                        table
                    }))
                }

                pub fn set_table(&mut self, table: $table) {
                    self.0 = table.as_u64() | PRESENT;
                }

                fn is_table(&self) -> bool {
                    self.0 & (PAGE_SIZE | PRESENT) == PRESENT
                }

                fn is_present(&self) -> bool {
                    self.0 & PRESENT == PRESENT
                }

                fn addr_table(&self) -> u64 {
                    self.0 & !0xfff
                }

                fn addr_page(&self) -> u64 {
                    self.0 & $mask
                }

                fn addr_table_ptr<T>(&self) -> *mut T {
                    ptr::with_exposed_provenance_mut(self.addr_table() as usize)
                }
            }
        };
    }

    impl_p!(T Pt PtEntry);
    impl_p!(T Pd PdEntry);
    impl_p!(T Pdp PdpEntry);
    impl_p!(T Pml4 Pml4Entry);
    impl_p!(E Pml4Entry Pdp 0x000fffff_c0000000);
    impl_p!(E PdpEntry  Pd  0x000fffff_ffe00000);
    impl_p!(E PdEntry   Pt  0x000fffff_fffff000);

    const PRESENT: u64 = 1 << 0;
    const READ_WRITE: u64 = 1 << 1;
    const PAGE_SIZE: u64 = 1 << 7;
    const EXECUTE_DISABLE: u64 = 1 << 63;

    impl PtEntry {
        fn set(&mut self, addr: u64, flags: u64) {
            self.0 = addr | flags;
        }

        pub fn set_r(&mut self, addr: u64) {
            self.set(addr, EXECUTE_DISABLE | PRESENT);
        }

        pub fn set_rw(&mut self, addr: u64) {
            self.set(addr, EXECUTE_DISABLE | READ_WRITE | PRESENT);
        }

        pub fn set_rx(&mut self, addr: u64) {
            self.set(addr, PRESENT);
        }

        pub fn set_rwx(&mut self, addr: u64) {
            self.set(addr, READ_WRITE | PRESENT);
        }
    }

    impl PdEntry {
        fn set(&mut self, addr: u64, flags: u64) {
            self.0 = addr | PAGE_SIZE | flags;
        }

        pub fn set_r(&mut self, addr: u64) {
            self.set(addr, EXECUTE_DISABLE | PRESENT);
        }

        pub fn set_rw(&mut self, addr: u64) {
            self.set(addr, EXECUTE_DISABLE | READ_WRITE | PRESENT);
        }

        pub fn set_rx(&mut self, addr: u64) {
            self.set(addr, PRESENT);
        }

        pub fn set_rwx(&mut self, addr: u64) {
            self.set(addr, READ_WRITE | PRESENT);
        }
    }

    const MASK_4K: u64 = (1<<12)-1;
    const MASK_2M: u64 = (1<<21)-1;

    pub fn identity_map_rw(range: ops::Range<NonNull<u8>>, alloc: &mut alloc::Allocator) {
        let mut start = range.start.addr().get() as u64 & !0xfff;
        let end = (range.end.addr().get() + 0xfff) as u64 & !0xfff;
        while start < end {
            let [_, i] = split_bits(start as usize, 12);
            let [pt_i, i] = split_bits(i, 9);
            let [pd_i, i] = split_bits(i, 9);
            let [pdp_i, i] = split_bits(i, 9);
            let [pml4_i, i] = split_bits(i, 9);
            assert_eq!(i, 0);
            let mut pml4 = unsafe { sys::pml4() };
            let Some(mut pdp) = pml4[pml4_i].get_or_alloc_table(alloc) else { fail("identity map PDP conflict") };
            let Some(mut pd) = pdp[pdp_i].get_or_alloc_table(alloc) else { fail("identity map PD conflict") };
            let mut pd: Pd = pd;
            if start & MASK_2M == 0 && end - start > MASK_2M {
                pd[pd_i].set_rw(start);
                start += MASK_2M + 1;
            } else {
                let Some(mut pt) = pd[pd_i].get_or_alloc_table(alloc) else { fail("identity map PT conflict") };
                pt[pt_i].set_rw(start);
                start += MASK_4K + 1;
            }
        }
    }
}

mod elf {
    use crate::*;

    struct Header {
        program_entry: usize,
        ph_offset: usize,
        ph_size: u16,
        ph_count: u16,
    }

    fn bytes_to_u64le(b: &[u8]) -> u64 {
        u64::from_le_bytes(b.try_into().expect("b.len() should be 8"))
    }

    fn bytes_to_u32le(b: &[u8]) -> u32 {
        u32::from_le_bytes(b.try_into().expect("b.len() should be 4"))
    }

    fn bytes_to_u16le(b: &[u8]) -> u16 {
        u16::from_le_bytes(b.try_into().expect("b.len() should be 2"))
    }

    fn parse_header(file: &[u8]) -> Header {
        let fail = |s| -> ! {
            sys::print("invalid ELF header: ");
            fail(s);
        };
        let assert = |c: bool, s| if !c { fail(s) };
        assert(file.len() >= 64, "truncated");
        assert(&file[..4] == b"\x7fELF", "bad magic");
        assert(file[4] == 2, "not 64 bit");
        assert(file[5] == 1, "not little endian");
        assert(file[16] == 3, "not PIE");
        let program_entry = bytes_to_u64le(&file[24..32]) as usize;
        let ph_offset = bytes_to_u64le(&file[32..40]) as usize;
        let ph_size = bytes_to_u16le(&file[54..56]);
        let ph_count = bytes_to_u16le(&file[56..58]);
        Header { program_entry, ph_offset, ph_size, ph_count }
    }

    const PH_TY_LOAD: u32 = 1;
    const PH_TY_DYNAMIC: u32 = 2;

    fn slice(data: &[u8], from: usize, len: usize) -> Option<&[u8]> {
        data.get(from..).and_then(|s| s.get(..len))
    }

    fn slice_fail<'a>(data: &'a [u8], from: usize, len: usize, msg: &str) -> &'a [u8] {
        slice(data, from, len).unwrap_or_else(|| fail(msg))
    }

    fn parse_program_headers(file: &[u8], alloc: &mut alloc::Allocator, header: &Header) -> NonNull<u8> {
        let fail = |s| -> ! {
            sys::print("parse_ph: ");
            fail(s);
        };
        let assert = |c: bool, s| if !c { fail(s) };
        assert(header.ph_size >= 56, "PH entry size smaller than expected");
        let Some(mut pt) = page::Pt::new(alloc) else {
            fail("out of memory");
        };
        let ph = slice_fail(file, header.ph_offset, usize::from(header.ph_size) * usize::from(header.ph_count), "PH array truncated");
        for e in ph.chunks_exact(header.ph_size.into()) {
            let ty = bytes_to_u32le(&e[0..4]);
            let flags = bytes_to_u32le(&e[4..8]);
            let offset = bytes_to_u64le(&e[8..16]);
            let vaddr = bytes_to_u64le(&e[16..24]);
            let filesz = bytes_to_u64le(&e[32..40]);
            let memsz = bytes_to_u64le(&e[40..48]);
            let align = bytes_to_u64le(&e[48..56]);
            match ty {
                PH_TY_LOAD => {
                    assert(align == alloc::PAGE_SIZE as u64, "unsupported alignment");
                    assert(offset % align == vaddr % align, "segment misaligned");
                    let mask = align - 1;
                    let mut va = vaddr & !mask;
                    let mut pa = file.as_ptr().addr() as u64 + (offset & !mask);
                    let map_n = (filesz + mask) / align;
                    let zero_n = (memsz + mask) / align;
                    for pi in 0..zero_n {
                        let [_, i] = split_bits(va as usize, 12);
                        let Some(pte) = pt.get_mut(i) else {
                            fail("vaddr above 2MiB not supported");
                        };
                        let p = if pi < map_n {
                            pa
                        } else {
                            alloc.alloc(alloc::PAGE_SIZE)
                                .unwrap_or_else(|| fail("ELF: out of memory"))
                                .addr()
                                .get() as u64
                        };
                        match flags {
                            0b100 => pte.set_r(p),
                            0b101 => pte.set_rx(p),
                            0b110 => pte.set_rw(p),
                            0b111 => pte.set_rwx(p),
                            _ => fail("unsupported flags"),
                        }
                        va += alloc::PAGE_SIZE as u64;
                        pa += alloc::PAGE_SIZE as u64;
                    }
                }
                PH_TY_DYNAMIC => {
                    let dynamic = slice_fail(file, offset as usize, filesz as usize, "dynamic segment truncated");
                    let mut rela @ mut relasz @ mut relaent = 0;
                    for e in dynamic.chunks_exact(8 * 3) {
                        let ty = bytes_to_u64le(&e[0..8]);
                        let val = bytes_to_u64le(&e[8..16]);
                        let ptr = bytes_to_u64le(&e[16..24]);
                        const RELA: u64 = 7;
                        const RELASZ: u64 = 8;
                        const RELAENT: u64 = 9;
                        match ty {
                            RELA => rela = val,
                            RELASZ => relasz = val,
                            RELAENT => relaent = val,
                            _ => {}
                        }
                    }
                    if relasz != 0 {
                        fail("TODO: PH_DYNAMIC: RELA\n");
                    }
                }
                _ => {}
            }
        }
        let Some(mut pd) = page::Pd::new(alloc) else { fail("out of memory") };
        pd[0].set_table(pt);
        let Some(mut pdp) = page::Pdp::new(alloc) else { fail("out of memory") };
        pdp[0].set_table(pd);
        let mut pml4 = unsafe { sys::pml4() };
        pml4[256].set_table(pdp);
        NonNull::new((usize::MAX << (9*4 + 12 - 1)) as *mut _).unwrap()
    }

    /// # Returns
    ///
    /// Entry point
    pub fn load(file: &[u8], alloc: &mut alloc::Allocator) -> NonNull<u8> {
        let hdr = parse_header(file);
        let base = parse_program_headers(file, alloc, &hdr);
        unsafe { base.add(hdr.program_entry) }
    }
}

mod pci {
    use crate::*;

    #[derive(Clone, Copy)]
    pub struct BDF(u16);

    impl BDF {
        pub const fn new(bus: u8, dev: u8, func: u8) -> Self {
            assert!(func < 8);
            assert!(dev < 32);
            Self(u16::from_be_bytes([bus, dev << 3 | func]))
        }

        pub const fn with_function(self, function: u8) -> Self {
            Self::new(self.bus(), self.dev(), function)
        }

        pub const fn bus(self) -> u8 {
            (self.0 >> 8) as u8
        }

        pub const fn dev(self) -> u8 {
            self.0 as u8 >> 3
        }

        pub const fn function(self) -> u8 {
            self.0 as u8 & 7
        }

        pub const fn index(self) -> u16 {
            self.0
        }
    }

    const PORTIO_CMD: u16 = 0xcf8;
    const PORTIO_DATA: u16 = 0xcfc;

    fn set_target(bdf: BDF, offset: u8) {
        let addr = 1 << 31 | u32::from(bdf.0) << 8 | u32::from(offset & !3);
        unsafe { x86::out32(PORTIO_CMD, addr) };
    }

    pub unsafe fn write32<const OFFSET: u8>(bdf: BDF, value: u32) {
        const { assert!(OFFSET & 3 == 0, "offset must be a multiple of 4") };
        set_target(bdf, OFFSET);
        unsafe { x86::out32(PORTIO_DATA, value) };
    }
}

mod pcie {
    use crate::*;
    use core::ptr::NonNull;

    struct Q35 {
    }

    #[allow(dead_code)]
    #[repr(C)]
    struct Header0 {
        device_id: VolatileCell<u16>,
        vendor_id: VolatileCell<u16>,
        status: VolatileCell<u16>,
        command: VolatileCell<u16>,
        class_code: VolatileCell<u8>,
        subclass: VolatileCell<u8>,
        prog_if: VolatileCell<u8>,
        revision_id: VolatileCell<u8>,
        bist: VolatileCell<u8>,
        header_type: VolatileCell<u8>,
        latency_timer: VolatileCell<u8>,
        cache_line_size: VolatileCell<u8>,
        bar: [VolatileCell<u32>; 6],
        cardbus_cis_pointer: VolatileCell<u32>,
        subsystem_id: VolatileCell<u16>,
        subsystem_vendor_id: VolatileCell<u16>,
        expansion_rom_address: VolatileCell<u32>,
        _reserved_0: [u8; 3],
        capabilities_pointer: VolatileCell<u8>,
        _reserved_1: [u8; 4],
        max_latency: VolatileCell<u8>,
        min_grant: VolatileCell<u8>,
        interrupt_pin: VolatileCell<u8>,
        interrupt_line: VolatileCell<u8>,
    }

    impl Q35 {
        const BASE: NonNull<u8> = unsafe { NonNull::new_unchecked(0xb000_0000u32 as _) };
        const SIZE: usize = 4096 * 8 * 32 * 256;
        const BDF: pci::BDF = pci::BDF::new(0, 0, 0);
        const CFG_PCIE_BASE: u8 = 0x60;

        pub fn range(&self) -> core::ops::Range<NonNull<u8>> {
            let start = Self::BASE;
            let end = unsafe { Self::BASE.byte_add(Self::SIZE) };
            start..end
        }

        fn enable(&self) {
            // based on SeaBIOS
            unsafe {
                let base = Self::BASE.addr().get() as u64;
                unsafe fn f<const O: u8>(x: u32) {
                    // no const generics? :(((
                    match O {
                        0 => pci::write32::<{ Q35::CFG_PCIE_BASE + 0 }>(Q35::BDF, x),
                        4 => pci::write32::<{ Q35::CFG_PCIE_BASE + 4 }>(Q35::BDF, x),
                        _ => panic!("no const generics! :((((((("),
                    }
                }
                f::<0>(0);
                f::<4>((base >> 32) as u32);
                f::<0>(base as u32 | 1);
            }
        }

        fn get_header(&self, bdf: pci::BDF) -> &Header0 {
            unsafe { Self::BASE.byte_add(4096 * usize::from(bdf.index())).cast::<Header0>().as_ref() }
        }

        fn configure_device(&self, bdf: pci::BDF) {
            let hdr = self.get_header(bdf);
            match hdr.vendor_id.get() {
                _ => {}
            }
        }

        fn configure_bus(&self, bus: u8) {
            for dev in 0..32 {
                self.configure_device(pci::BDF::new(bus, dev, 0));
            }
        }

        pub fn configure(&self) {
            self.enable();
            self.configure_bus(0);
        }
    }

    pub fn configure(alloc: &mut alloc::Allocator) {
        let host = Q35 {};
        page::identity_map_rw(host.range(), alloc);
        host.configure();
    }
}

struct VolatileCell<T>(UnsafeCell<T>);

impl<T> VolatileCell<T>
where
    T: Copy
{
    fn get(&self) -> T {
        unsafe { core::ptr::read_volatile(self.0.get()) }
    }

    fn set(&self, value: T) {
        unsafe { core::ptr::write_volatile(self.0.get(), value) }
    }
}

fn split_bits(x: usize, bit: u8) -> [usize; 2] {
    [x & ((1 << bit) - 1), x >> bit]
}

fn fail(reason: &str) -> ! {
    sys::print(reason);
    sys::print("\n");
    sys::exit_err();
}

/// # Note
///
/// Start of file is guaranteed to be aligned to page boundary.
fn load_file<'a>(filename: &str, alloc: &mut alloc::Allocator) -> &'a [u8] {
    let len = match sys::open(filename) {
        Ok(x) => x,
        Err(sys::OpenFileError) => {
            sys::print("failed to open ");
            fail(filename);
        }
    };
    let Some(base) = alloc.alloc(len) else {
        fail("out of memory while reading file")
    };
    let buf: &mut [u8] = unsafe { core::slice::from_raw_parts_mut(base.as_ptr(), len) };
    sys::read(buf);
    buf
}

#[unsafe(no_mangle)]
extern "sysv64" fn boot() -> NonNull<u8> {
    let alloc = &mut alloc::Allocator::new();
    let pcie_base = pcie::configure(alloc);
    let file = load_file(KERNEL_FILENAME, alloc);
    elf::load(file, alloc)
}
