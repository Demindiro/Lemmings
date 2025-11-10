use crate::{
    KernelEntryToken, page,
    sync::{SpinLock, SpinLockGuard},
    thread::{self, RoundRobinQueue, ThreadHandle},
};
use core::{arch::naked_asm, mem::MaybeUninit};
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

static IRQ_HANDLERS: SpinLock<IrqHandlers> = SpinLock::new(IrqHandlers::new());

const IDT_NR: usize = 256;
// 32 reserved + 1 for timer
const VECTOR_STUB_OFFSET: u8 = 33;
const VECTOR_TIMER: u8 = 32;
const VECTOR_NR: usize = IDT_NR - VECTOR_STUB_OFFSET as usize;

pub mod door {
    use lemmings_idl_interrupt::*;

    door! {
        [lemmings_idl_interrupt Interrupt "x86-64 interrupt"]
        wait
        done
        reserve
        release
        map
    }

    fn wait(x: IrqVector) {
        let IrqVector { irq, vector } = x;
        let irq = u32::from(irq).try_into().expect("invalid IRQ");
        let vector = u32::from(vector).try_into().expect("invalid vector");
        critical_section::with(|cs| {
            let h = super::IRQ_HANDLERS.lock(cs);
            super::IrqHandlers::wait(h, vector, cs);
            let mut h = super::IRQ_HANDLERS.lock(cs);
            h.mask(irq);
            h.eoi();
        });
    }

    fn done(x: IrqVector) {
        let IrqVector { irq, .. } = x;
        let x = u32::from(irq).try_into().expect("invalid IRQ");
        critical_section::with(|cs| super::IRQ_HANDLERS.lock(cs).unmask(x));
    }

    fn reserve() -> MaybeVector {
        critical_section::with(|cs| super::IRQ_HANDLERS.lock(cs).reserve())
            .map(u32::from)
            .map_or_else(
                || MaybeVector::NoVector,
                |x| MaybeVector::Vector(Vector::try_from(u32::from(x)).unwrap()),
            )
    }

    fn release(x: Vector) {
        let x = u32::from(x).try_into().expect("invalid vector");
        critical_section::with(|cs| super::IRQ_HANDLERS.lock(cs).release(x));
    }

    fn map(x: Map) {
        let Map { irq, vector, mode } = x;
        let irq = u32::from(irq).try_into().expect("invalid IRQ");
        let vector = u32::from(vector).try_into().expect("invalid vector");
        let edge = match mode {
            TriggerMode::Level => false,
            TriggerMode::Edge => true,
        };
        critical_section::with(|cs| super::IRQ_HANDLERS.lock(cs).map(irq, vector, edge));
    }
}

pub struct Msi {
    pub data: u32,
    pub address: mmu::Phys<mmu::A2>,
    pub vector: u32,
}

struct IrqHandlers {
    queues: [RoundRobinQueue; VECTOR_NR],
    allocated: [u32; (VECTOR_NR + 32) / 32],
}

impl IrqHandlers {
    const fn new() -> Self {
        Self {
            queues: [const { RoundRobinQueue::new() }; VECTOR_NR],
            allocated: [0; (VECTOR_NR + 32) / 32],
        }
    }

    fn wait(mut slf: SpinLockGuard<Self>, vector: u8, cs: CriticalSection<'_>) {
        let vector = usize::from(vector - VECTOR_STUB_OFFSET);
        slf.queues[vector].enqueue_last(thread::current());
        drop(slf);
        thread::park(cs);
    }

    fn dequeue(&mut self, vector: u8) -> Option<ThreadHandle> {
        let vector = usize::from(vector - VECTOR_STUB_OFFSET);
        self.queues[vector].dequeue_first()
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
        todo!();
    }

    fn map(&mut self, irq: u8, vector: u8, edge: bool) {
        // FIXME detect Local APIC ID
        let mode = if edge {
            TriggerMode::Edge
        } else {
            TriggerMode::Level
        };
        unsafe { self.ioapic().set_irq(irq, 0, vector, mode, false) }
    }

    fn mask(&mut self, irq: u8) {
        unsafe { self.ioapic().mask_irq(irq, true) }
    }

    fn unmask(&mut self, irq: u8) {
        unsafe { self.ioapic().mask_irq(irq, false) }
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

pub fn alloc_msi() -> Option<Msi> {
    let vector = critical_section::with(|cs| IRQ_HANDLERS.lock(cs).reserve()).map(u32::from)?;
    Some(Msi {
        data: vector,
        address: apic::local::DEVICE_LAPIC_ADDRESS.into(),
        vector,
    })
}

pub fn wait_msi(vector: u32) {
    let vector = vector.try_into().expect("invalid vector");
    critical_section::with(|cs| {
        let h = IRQ_HANDLERS.lock(cs);
        IrqHandlers::wait(h, vector, cs);
        let mut h = IRQ_HANDLERS.lock(cs);
        // TODO should we mask? If we do, we'll need done_msi too.
        // We also need to keep track of IRQ too then... annoyances.
        //h.mask(irq);
        h.eoi();
    });
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
            let p = &irq_stub_table as *const [u8; 5];
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

extern "sysv64" fn irq_handler<'a>(irq: u8) {
    // SAFETY: interrupts are disabled right now
    let cs = unsafe { CriticalSection::<'a>::new() };
    let thread = IRQ_HANDLERS.lock(cs).dequeue(irq);
    let thread = thread.unwrap_or_else(|| todo!("no waiting threads"));
    thread.resume(cs);
}

extern "sysv64" fn timer_handler() {
    todo!("timer handler");
}

unsafe extern "sysv64" {
    static irq_stub_table: [[u8; 5]; 256 - VECTOR_STUB_OFFSET as usize];
}

// Generate 223 IRQ stubs which each push the IRQ number.
//
// This approach is used because it's very useful to avoid duplicating (machine) code for handlers
// that are shared between IRQs.
//
// We only generate 256 - 33 = 223 stubs because the first 33 IRQs have a well-defined purpose.
// It's only the dynamically assigned interrupts that are muddy.
core::arch::global_asm! {
    "irq_stub_table:",
    ".rept 256 - {VECTOR_STUB_OFFSET}",
    "call irq_entry",
    ".endr",
    "irq_entry:",
    // Save scratch registers
    // except rax, see below
    /* rip from irq_stub_table, will become rax */ // 0
    "push rdi", // 1
    "push rsi", // 2
    "push rdx", // 3
    "push rcx", // 4
    "push r8",  // 5
    "push r9",  // 6
    "push r10", // 7
    "push r11", // 8
    // xchg has an implicit lock, so it's horrendously slow.
    // Still, we can emulate it efficiently with a scratch register, which we'll need as
    // argument register anyways :)
    "mov rdi, [rsp + 8 * 8]", // load caller *next* rip
    "mov [rsp + 8 * 8], rax", // store rax
    // offset in handler table is (rip - (irq_stub_table + 5)) / 5
    // account for VECTOR_STUB_OFFSET while at it
    "lea rcx, [rip + irq_stub_table + 5 - ({VECTOR_STUB_OFFSET} * 5)]",
    "sub edi, ecx",
    // The trick here is to find some large enough power-of-two divisor, then find the corresponding
    // dividend to approach 1/5, i.e. divisor / 5 = dividend.
    // In this case: 1/5 * 1024 = 204.8
    "imul edi, edi, 205",
    "shr edi, 10",
    // Call handler
    "cld",
    "call {irq_handler}",
    // Restore thread state
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
    irq_handler = sym irq_handler,
}
