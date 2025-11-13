use crate::{
    KernelEntryToken, page,
    sync::{SpinLock, SpinLockGuard},
    thread::{self, ThreadHandle},
};
use core::{arch::naked_asm, mem::MaybeUninit, num::NonZero};
use critical_section::CriticalSection;
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
        subscribe
        unsubscribe
        done
    }

    fn subscribe(Subscribe { irq, mode }: Subscribe) -> SubscribeResult {
        debug!("irq subscribe {irq:?} {mode:?}");
        let irq = irqnr(irq);
        let edge = matches!(mode, TriggerMode::Edge);
        let res = critical_section::with(|cs| INTERRUPT_HANDLERS.lock(cs).subscribe_irq(irq, edge));
        debug!("irq subscribe -> {res:?}");
        match res {
            Result::Ok(()) => Ok.into(),
            Result::Err(SubscribeIrqError::AlreadySubscribed) => AlreadySubscribed.into(),
            Result::Err(_) => Fail.into(),
        }
    }

    fn unsubscribe(irq: IrqNr) {
        debug!("irq unsubscribe {irq:?}");
        let irq = irqnr(irq);
        critical_section::with(|cs| INTERRUPT_HANDLERS.lock(cs).unsubscribe_irq(irq));
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
#[derive(Clone, Copy)]
struct VectorNr(NonZero<u8>);

#[derive(Debug)]
struct InvalidIrqNr;
#[derive(Debug)]
struct InvalidVectorNr;

#[derive(Debug)]
enum SubscribeIrqError {
    OutOfVectors,
    AlreadySubscribed,
}

struct InterruptHandlers {
    subscribed: [Option<ThreadHandle>; VECTOR_NR],
    allocated: [u32; (VECTOR_NR + 32) / 32],
    vector_to_irq: [u8; VECTOR_NR],
}

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

impl From<OutOfVectors> for SubscribeIrqError {
    fn from(_: OutOfVectors) -> Self {
        SubscribeIrqError::OutOfVectors
    }
}

impl InterruptHandlers {
    const fn new() -> Self {
        Self {
            subscribed: [const { None }; VECTOR_NR],
            allocated: [0; (VECTOR_NR + 32) / 32],
            vector_to_irq: [0; VECTOR_NR],
        }
    }

    fn subscribe(&mut self) -> Result<u8, OutOfVectors> {
        let vector = self.reserve().ok_or(OutOfVectors)?;
        let i = usize::from(vector - VECTOR_STUB_OFFSET);
        self.subscribed[i] = Some(thread::current());
        Ok(vector)
    }

    fn unsubscribe(&mut self, vector: u8) {
        let i = usize::from(vector - VECTOR_STUB_OFFSET);
        self.subscribed[i] = None;
        self.release(vector);
    }

    fn subscribe_irq(&mut self, irq: IrqNr, edge: bool) -> Result<(), SubscribeIrqError> {
        if self.vector_to_irq.contains(&irq.0) {
            return Err(SubscribeIrqError::AlreadySubscribed);
        }
        let vector = self.subscribe()?;
        let i = usize::from(vector - VECTOR_STUB_OFFSET);
        self.map(irq, vector, edge);
        self.vector_to_irq[i] = irq.0;
        Ok(())
    }

    fn unsubscribe_irq(&mut self, irq: IrqNr) {
        let i = self
            .vector_to_irq
            .iter()
            .position(|x| *x == irq.0)
            .expect("unmapped irq");
        self.vector_to_irq[i] = 0xff;
        self.unsubscribe(VECTOR_STUB_OFFSET + i as u8);
    }

    fn notify(&self, vector: u8) {
        let i = usize::from(vector - VECTOR_STUB_OFFSET);
        self.subscribed[i].as_ref().map(|x| x.notify());
    }

    fn reserve(&mut self) -> Option<u8> {
        for (i, n) in self.allocated.iter_mut().enumerate() {
            if *n == u32::MAX {
                continue;
            }
            let b = n.trailing_ones() as usize;
            *n |= 1 << b;
            return Some(VECTOR_STUB_OFFSET + (i * 32 + b) as u8);
        }
        None
    }

    fn release(&mut self, vector: u8) {
        let vector = usize::from(vector - VECTOR_STUB_OFFSET);
        let [i, b] = [vector / 32, vector % 32];
        self.allocated[i] &= !(1 << b);
        self.vector_to_irq[i] = 0xff;
    }

    fn map(&mut self, irq: IrqNr, vector: u8, edge: bool) {
        // FIXME detect Local APIC ID
        let mode = if edge {
            TriggerMode::Edge
        } else {
            TriggerMode::Level
        };
        unsafe { self.ioapic().set_irq(irq.0, 0, vector, mode, false) }
    }

    fn mask_irq(&mut self, irq: IrqNr) {
        unsafe { self.ioapic().mask_irq(irq.0, true) }
    }

    fn unmask_irq(&mut self, irq: IrqNr) {
        unsafe { self.ioapic().mask_irq(irq.0, false) }
    }

    fn mask_vector(&mut self, vector: u8) {
        let i = usize::from(vector - VECTOR_STUB_OFFSET);
        let irq = self.vector_to_irq[i];
        if irq != 0xff {
            self.mask_irq(IrqNr(irq));
        }
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
}

pub fn subscribe_msi() -> Result<Msi, OutOfVectors> {
    let vector = critical_section::with(|cs| INTERRUPT_HANDLERS.lock(cs).subscribe())?;
    Ok(Msi {
        data: vector.into(),
        address: apic::local::DEVICE_LAPIC_ADDRESS.into(),
    })
}

pub fn unsubscribe_msi(msi: Msi) {
    let vector = (msi.data & 0xff).try_into().expect("invalid vector");
    critical_section::with(|cs| INTERRUPT_HANDLERS.lock(cs).unsubscribe(vector));
}

pub fn init(token: KernelEntryToken) -> KernelEntryToken {
    unsafe { pic::init() };
    let root = unsafe { mmu::current_root::<mmu::L4>() };
    init_gdt(&root);
    init_idt(&root);
    init_apic();
    token
}

#[inline(always)]
fn init_gdt(root: &mmu::Root<mmu::L4>) {
    let stack_top = 0x1000 as *const usize;
    unsafe { (&mut *(&raw mut TSS)).set_ist(1.try_into().unwrap(), stack_top) };
    #[allow(static_mut_refs)]
    unsafe {
        GDT.set_tss(&TSS)
    };
    let gdtp = (&raw const GDT).addr() as u64;
    let gdtp = mmu::Virt::new(gdtp).expect("GDT should be in valid virtual space");
    let gdtp = root
        .translate(&page::IdentityMapper, gdtp)
        .expect("GDT should be mapped");
    unsafe { GdtPointer::new(gdtp).activate() };
}

#[inline(always)]
fn init_idt(root: &mmu::Root<mmu::L4>) {
    let idt = unsafe { &mut *(&raw mut IDT) };

    let mut ist1 = |f| IdtEntry::new(Gdt::KERNEL_CS, f, Ist::N1);
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
    let idtp = root
        .translate(&page::IdentityMapper, idtp)
        .expect("IDT should be mapped");
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

extern "sysv64" fn interrupt_handler<'a>(vector: u8) {
    debug!("interrupt {vector}");
    // SAFETY: interrupts are disabled right now
    let cs = unsafe { CriticalSection::<'a>::new() };
    let mut h = INTERRUPT_HANDLERS.lock(cs);
    h.notify(vector);
    h.mask_vector(vector);
    h.eoi();
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
    "movzx edi, al",
    "cld",
    "call {interrupt_handler}",
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
