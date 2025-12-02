use crate::{
    KernelEntryToken, page,
    thread::{self, RoundRobinQueue},
};
use core::{arch::naked_asm, mem::MaybeUninit, num::NonZero};
use critical_section::CriticalSection;
use lemmings_spinlock::{SpinLock, SpinLockGuard};
use lemmings_x86_64::{
    apic::{
        self,
        io::{IoApicHelper, TriggerMode},
        local::LocalApicHelper,
    },
    gdt::{Gdt, GdtPointer},
    idt::{self, Idt, IdtEntry, IdtPointer, Ist},
    mmu, pic,
    tss::Tss,
};

static mut GDT: Gdt = Gdt::new();
static mut TSS: Tss = Tss::new();
static mut IDT: Idt<IDT_NR> = Idt::new();

static mut LOCAL_APIC: MaybeUninit<LocalApicHelper<'static>> = MaybeUninit::uninit();

static INTERRUPT_HANDLERS: SpinLock<InterruptHandlers> = SpinLock::new(InterruptHandlers::new());

const IDT_NR: usize = 256;
// 32 reserved + 1 for timer
const VECTOR_STUB_OFFSET: u8 = 33;
const VECTOR_TIMER: u8 = 32;
const VECTOR_NR: usize = IDT_NR - VECTOR_STUB_OFFSET as usize;

pub mod door {
    use super::*;
    use lemmings_idl_irq::{IrqNr, TriggerMode, *};

    door! {
        [lemmings_idl_irq Irq "x86-64 IRQ"]
        map
        unmap
        wait
        done
    }

    fn map(Map { irq, mode }: Map) -> MapResult {
        debug!("irq map {irq:?} {mode:?}");
        let irq = irqnr(irq);
        let edge = matches!(mode, TriggerMode::Edge);
        let res = critical_section::with(|cs| {
            let mut h = INTERRUPT_HANDLERS.lock(cs);
            let vector = h.alloc_vector()?;
            h.map_irq(irq, vector, edge);
            Result::Ok(vector)
        });
        debug!("irq map -> {res:?}");
        match res {
            Result::Ok(_) => Ok.into(),
            Result::Err(super::OutOfVectors) => Fail.into(),
        }
    }

    fn unmap(irq: IrqNr) {
        debug!("irq unmap {irq:?}");
        let irq = irqnr(irq);
        critical_section::with(|cs| {
            let mut h = INTERRUPT_HANDLERS.lock(cs);
            let vector = h.irq_to_vector(irq).expect("unmapped irq");
            h.unmap_irq(irq);
            h.free_vector(vector);
        });
    }

    fn wait(irq: IrqNr) {
        debug!("irq wait {irq:?}");
        let irq = irqnr(irq);
        critical_section::with(|cs| {
            let h = INTERRUPT_HANDLERS.lock(cs);
            let vector = h.irq_to_vector(irq).expect("unmapped irq");
            InterruptHandlers::wait(h, vector);
        });
    }

    fn done(irq: IrqNr) {
        debug!("irq done {irq:?}");
        let irq = irqnr(irq);
        critical_section::with(|cs| INTERRUPT_HANDLERS.lock(cs).unmask_irq(irq));
    }

    #[track_caller]
    #[inline(always)]
    fn irqnr(irq: impl Into<u32>) -> super::IrqNr {
        super::IrqNr::try_from(irq.into()).expect("invalid IRQ")
    }
}

pub struct Msi {
    pub data: u32,
    pub address: mmu::Phys<mmu::A2>,
}

#[derive(Debug)]
pub struct OutOfVectors;

#[derive(Clone, Copy)]
struct IrqNr(u8);
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
struct VectorNr(NonZero<u8>);

#[derive(Debug)]
struct InvalidIrqNr;
#[derive(Debug)]
struct InvalidVectorNr;

struct InterruptHandlers {
    queues: [RoundRobinQueue; VECTOR_NR],
    allocated: BitArray<VECTOR_NR, 7>,
    signaled: BitArray<VECTOR_NR, 7>,
    vector_to_irq: [u8; VECTOR_NR],
}

struct BitArray<const N: usize, const WORDS: usize>([u32; WORDS]);

impl TryFrom<u32> for IrqNr {
    type Error = InvalidIrqNr;

    fn try_from(x: u32) -> Result<Self, Self::Error> {
        // TODO figure out actual limits
        u8::try_from(x)
            .ok()
            .filter(|x| (0..255).contains(x))
            .map(Self)
            .ok_or(InvalidIrqNr)
    }
}

impl TryFrom<u32> for VectorNr {
    type Error = InvalidVectorNr;

    fn try_from(x: u32) -> Result<Self, Self::Error> {
        // Note that 0xff is reserved for spurious interrupt
        u8::try_from(x)
            .ok()
            .and_then(NonZero::new)
            .filter(|x| (32..255).contains(&x.get()))
            .map(Self)
            .ok_or(InvalidVectorNr)
    }
}

impl TryFrom<u8> for VectorNr {
    type Error = InvalidVectorNr;

    fn try_from(x: u8) -> Result<Self, Self::Error> {
        u32::from(x).try_into()
    }
}

impl From<VectorNr> for u8 {
    fn from(x: VectorNr) -> Self {
        x.0.get()
    }
}

impl From<VectorNr> for u32 {
    fn from(x: VectorNr) -> Self {
        u8::from(x).into()
    }
}

impl InterruptHandlers {
    const fn new() -> Self {
        Self {
            queues: [const { RoundRobinQueue::new() }; VECTOR_NR],
            allocated: BitArray([0; (VECTOR_NR + 32) / 32]),
            signaled: BitArray([0; (VECTOR_NR + 32) / 32]),
            vector_to_irq: [0; VECTOR_NR],
        }
    }

    fn wait(mut slf: SpinLockGuard<'_, '_, Self>, vector: VectorNr) {
        let i = Self::vector_to_i(vector);
        let cs = slf.cs;
        slf.queues[i].enqueue(cs, thread::current());
        drop(slf);
        thread::park(cs);
    }

    fn handle(mut slf: SpinLockGuard<'_, '_, Self>, vector: VectorNr) {
        let i = Self::vector_to_i(vector);
        let cs = slf.cs;
        if let Some(thread) = slf.queues[i].dequeue(cs) {
            slf.eoi();
            drop(slf);
            thread::unpark(cs, thread);
        } else {
            slf.mask_vector(vector);
            slf.signaled.set(i, true);
            slf.eoi();
        }
    }

    fn alloc_vector(&mut self) -> Result<VectorNr, OutOfVectors> {
        self.allocated
            .alloc()
            .map(Self::i_to_vector)
            .ok_or(OutOfVectors)
    }

    fn free_vector(&mut self, vector: VectorNr) {
        let i = Self::vector_to_i(vector);
        assert!(self.allocated.get(i), "vector is not allocated");
        self.allocated.set(i, false);
        self.vector_to_irq[i] = 0xff;
    }

    fn map_irq(&mut self, irq: IrqNr, vector: VectorNr, edge: bool) {
        // FIXME detect Local APIC ID
        let mode = if edge {
            TriggerMode::Edge
        } else {
            TriggerMode::Level
        };
        let i = Self::vector_to_i(vector);
        self.vector_to_irq[i] = irq.0;
        unsafe { self.ioapic().set_irq(irq.0, 0, vector.into(), mode, false) }
    }

    fn unmap_irq(&mut self, irq: IrqNr) {
        unsafe {
            self.ioapic()
                .set_irq(irq.0, 0, 0xff, TriggerMode::Edge, false)
        }
        //let i = Self::vector_to_i(vector);
        //self.vector_to_irq[i] = 0xff;
    }

    fn mask_irq(&mut self, irq: IrqNr) {
        unsafe { self.ioapic().mask_irq(irq.0, true) }
    }

    fn unmask_irq(&mut self, irq: IrqNr) {
        unsafe { self.ioapic().mask_irq(irq.0, false) }
    }

    fn mask_vector(&mut self, vector: VectorNr) {
        let i = Self::vector_to_i(vector);
        let irq = self.vector_to_irq[i];
        if irq != 0xff {
            self.mask_irq(IrqNr(irq));
        }
    }

    fn irq_to_vector(&self, irq: IrqNr) -> Option<VectorNr> {
        self.vector_to_irq
            .iter()
            .position(|x| *x == irq.0)
            .map(Self::i_to_vector)
    }

    fn eoi(&mut self) {
        let apic = unsafe { (&mut *(&raw mut LOCAL_APIC)).assume_init_ref() };
        apic.set_eoi(0);
    }

    fn ioapic(&self) -> IoApicHelper<'_> {
        let io = apic::io::DEFAULT_ADDR;
        let io = page::phys_to_virt(page::Phys(io.into()));
        let io = unsafe { io.cast::<apic::io::IoApic>().as_ref() };
        let io = IoApicHelper::new(io);
        io
    }

    fn vector_to_i(x: VectorNr) -> usize {
        usize::from(x.0.get() - VECTOR_STUB_OFFSET)
    }

    fn i_to_vector(i: usize) -> VectorNr {
        debug_assert!(i < VECTOR_NR);
        (VECTOR_STUB_OFFSET + i as u8)
            .try_into()
            .expect("invalid vector index")
    }
}

impl<const N: usize, const WORDS: usize> BitArray<N, WORDS> {
    fn get(&self, i: usize) -> bool {
        const {
            assert!(WORDS == (N + 31) / 32);
        }
        let [k, b] = [i / 32, i % 32];
        self.0[k] & 1 << b != 0
    }

    fn set(&mut self, i: usize, value: bool) {
        let [k, b] = [i / 32, i % 32];
        self.0[k] &= !(1 << b);
        self.0[k] |= u32::from(value) << b;
    }

    fn alloc(&mut self) -> Option<usize> {
        for (k, n) in self.0.iter_mut().enumerate() {
            if *n == u32::MAX {
                continue;
            }
            let b = n.trailing_ones() as usize;
            let i = 32 * k + b;
            return (i < N).then(|| {
                *n |= 1 << b;
                i
            });
        }
        None
    }
}

pub fn map_msi() -> Result<Msi, OutOfVectors> {
    let vector = critical_section::with(|cs| INTERRUPT_HANDLERS.lock(cs).alloc_vector())?;
    debug!("map_msi {vector:?}");
    Ok(Msi {
        data: vector.into(),
        address: apic::local::DEVICE_LAPIC_ADDRESS.into(),
    })
}

pub fn unmap_msi(msi: Msi) {
    let vector = (msi.data & 0xff).try_into().expect("invalid vector");
    debug!("unmap_msi {vector:?}");
    critical_section::with(|cs| INTERRUPT_HANDLERS.lock(cs).free_vector(vector));
}

pub fn wait_msi(msi: Msi) {
    let vector = (msi.data & 0xff).try_into().expect("invalid vector");
    debug!("wait_msi {vector:?}");
    critical_section::with(|cs| InterruptHandlers::wait(INTERRUPT_HANDLERS.lock(cs), vector));
}

pub fn init(token: KernelEntryToken) -> KernelEntryToken {
    unsafe { pic::init() };
    init_gdt();
    init_idt();
    init_apic();
    token
}

#[inline(always)]
fn init_gdt() {
    let stack_top = 0x1000 as *const usize;
    let tss = unsafe { &mut *(&raw mut TSS) };
    let gdt = unsafe { &mut *(&raw mut GDT) };
    unsafe { tss.set_ist(1.try_into().unwrap(), stack_top) };
    gdt.set_tss(tss);
    let gdtp = (&raw const GDT).addr() as u64;
    let gdtp = mmu::Virt::new(gdtp).expect("GDT should be in valid virtual space");
    unsafe { GdtPointer::new(gdtp).activate() };
}

#[inline(always)]
fn init_idt() {
    let idt = unsafe { &mut *(&raw mut IDT) };

    let ist1 = |f| IdtEntry::new(Gdt::KERNEL_CS, f, Ist::N1);
    idt.set(idt::nr::DOUBLE_FAULT, ist1(double_fault as _));
    idt.set_handler(idt::nr::PAGE_FAULT, page_fault as _);
    idt.set_handler(VECTOR_TIMER, timer_handler as _);
    for i in VECTOR_STUB_OFFSET..=u8::MAX {
        unsafe {
            let p = &irq_stub_table as *const [u8; 8];
            let p = p.add(usize::from(i - VECTOR_STUB_OFFSET));
            idt.set_handler(i, p.cast());
        }
    }

    let idtp = (&raw const IDT).addr() as u64;
    let idtp = mmu::Virt::new(idtp).expect("IDT should be in valid virtual space");
    unsafe { IdtPointer::new::<IDT_NR>(idtp).activate() };
}

#[inline(always)]
fn init_apic() {
    let apic = apic::local_address();
    let apic = page::phys_to_virt(page::Phys(apic.into()));
    let apic = unsafe { apic.cast::<apic::local::LocalApic>().as_ref() };
    let apic = LocalApicHelper::new(apic);
    let apic = unsafe { (&mut *(&raw mut LOCAL_APIC)).write(apic) };
    apic.enable();
}

extern "sysv64" fn double_fault() {
    panic!("Double fault!");
}

#[naked]
unsafe extern "C" fn page_fault() {
    unsafe {
        naked_asm! {
            "pop rsi",
            "mov rdi, [rsp]",
            "call {handler}",
            handler = sym handler,
        }
    }
    extern "sysv64" fn handler(rip: unsafe extern "C" fn(), error_code: u32) {
        let addr = lemmings_x86_64::cr2::get();
        panic!("Page fault! (rip: {rip:?}, code: {error_code:#08x}, address: {addr:#016x})");
    }
}

extern "sysv64" fn interrupt_handler<'a>(vector: VectorNr) {
    debug!("interrupt {vector:?}");
    // SAFETY: interrupts are disabled right now
    let cs = unsafe { CriticalSection::<'a>::new() };
    let h = INTERRUPT_HANDLERS.lock(cs);
    InterruptHandlers::handle(h, vector);
}

extern "sysv64" fn timer_handler() {
    todo!("timer handler");
}

unsafe extern "sysv64" {
    static irq_stub_table: [[u8; 8]; 256 - VECTOR_STUB_OFFSET as usize];
}

// Generate 223 IRQ stubs which each push the IRQ number.
//
// This approach is used because it's very useful to avoid duplicating (machine) code for handlers
// that are shared between IRQs.
//
// We only generate 256 - 33 = 223 stubs because the first 33 IRQs have a well-defined purpose.
// It's only the dynamically assigned interrupts that are muddy.
core::arch::global_asm! {
    ".align 8",
    "irq_stub_table:",
    ".rept 256 - {VECTOR_STUB_OFFSET}",
    // Interrupt handling ideally should be fast, so avoid messing up the RAS.
    // This means no trickery with call/pop, we need to match call/ret
    ".align 8",
    "push rax", // 0
    "mov al, \\+ + {VECTOR_STUB_OFFSET}",
    "jmp irq_entry",
    ".endr",
    "irq_entry:",
    "push rdi", // 1
    "push rsi", // 2
    "push rdx", // 3
    "push rcx", // 4
    "push r8",  // 5
    "push r9",  // 6
    "push r10", // 7
    "push r11", // 8
    "pushf",    // 9
    "movzx edi, al",
    "cld",
    "call {interrupt_handler}",
    "popf",    // 9
    "pop r11", // 8
    "pop r10", // 7
    "pop r9",  // 6
    "pop r8",  // 5
    "pop rcx", // 4
    "pop rdx", // 3
    "pop rsi", // 2
    "pop rdi", // 1
    "pop rax", // 0
    "iretq",
    VECTOR_STUB_OFFSET = const VECTOR_STUB_OFFSET,
    interrupt_handler = sym interrupt_handler,
}
