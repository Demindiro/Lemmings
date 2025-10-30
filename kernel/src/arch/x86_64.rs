use core::{mem::MaybeUninit, ptr::NonNull};
use crate::{KernelEntryToken, page, sync::SpinLock, thread::{self, RoundRobinQueue}};
use critical_section::CriticalSection;
use lemmings_x86_64::{apic::{self, local::LocalApicHelper, io::{IoApicHelper, TriggerMode}}, idt::{self, Idt, Ist, IdtPointer}, gdt::{Gdt, GdtPointer}, tss::Tss, mmu, pic};

static mut GDT: Gdt = Gdt::new();
static mut TSS: Tss = Tss::new();
static mut IDT: Idt<IDT_NR> = Idt::new();

static mut LOCAL_APIC: MaybeUninit<LocalApicHelper<'static>> = MaybeUninit::uninit();

static IRQ_HANDLERS: SpinLock<IrqHandlers> = SpinLock::new(IrqHandlers::new());

const IDT_NR: usize = 256;
// 32 reserved + 1 for timer
const IRQ_STUB_OFFSET: u8 = 33;
const IRQ_TIMER: u8 = 32;
const IRQ_NR: usize = IDT_NR - IRQ_STUB_OFFSET as usize;

pub mod door {
    use lemmings_idl_x86_64_interrupt::*;

    door! {
        [lemmings_idl_x86_64_interrupt Interrupt "x86-64 interrupt"]
        wait
        reserve_irq
        release_irq
    }

    fn wait(x: IrqN) -> Void {
        unsafe {
            core::arch::asm! {
                "pushf",
                "sti",
                "hlt",
                "popf",
            }
        }
        todo!();
    }

    fn reserve_irq(_: Void) -> MaybeIrqN {
        critical_section::with(|cs| {
            super::IRQ_HANDLERS
                .lock(cs)
                .reserve()
                .map_or(MaybeIrqN::None(None::default()), |x| MaybeIrqN::IrqN(IrqN::try_from(x).unwrap()))
        })
    }

    fn release_irq(x: IrqN) -> Void {
        critical_section::with(|cs| {
            super::IRQ_HANDLERS
                 .lock(cs)
                 .release(x.into());
            Void
        })
    }
}

struct IrqHandlers {
    queues: [RoundRobinQueue; IRQ_NR],
    allocated: [u32; (IRQ_NR + 32) / 32],
}

impl IrqHandlers {
    const fn new() -> Self {
        Self {
            queues: [const { RoundRobinQueue::new() }; IRQ_NR],
            allocated: [0; (IRQ_NR + 32) / 32],
        }
    }

    fn wait(&mut self, irq: u8, cs: CriticalSection<'_>) {
        let irq = usize::from(irq - IRQ_STUB_OFFSET);
        self.queues[irq].enqueue_last(thread::current());
        thread::park(cs);
    }

    fn reserve(&mut self) -> Option<u8> {
        for (i, n) in self.allocated.iter_mut().enumerate() {
            if *n == u32::MAX {
                continue;
            }
            let b = n.trailing_ones() as usize;
            *n |= 1 << b;
            return Some(IRQ_STUB_OFFSET + (i * 32 + b) as u8)
        }
        None
    }

    fn release(&mut self, irq: u8) {
        let irq = usize::from(irq) - IRQ_NR;
        let [i, b] = [irq / 32, irq % 32];
        self.allocated[i] &= !(1 << b);
        todo!();
    }
}

pub fn init(token: KernelEntryToken) -> KernelEntryToken {
    unsafe { pic::init() };
    let root = unsafe { mmu::current_root::<mmu::L4>() };
    init_gdt(&root);
    init_idt(&root);
    init_apic(&root);
    token
}

#[inline(always)]
fn init_gdt(root: &mmu::Root<mmu::L4>) {
    #[allow(static_mut_refs)]
    unsafe { GDT.set_tss(&TSS) };
    let gdtp = (&raw const GDT).addr() as u64;
    let gdtp = mmu::Virt::new(gdtp).expect("GDT should be in valid virtual space");
    let gdtp = root.translate(&page::IdentityMapper, gdtp).expect("GDT should be mapped");
    unsafe { GdtPointer::new(gdtp).activate() };
}

#[inline(always)]
fn init_idt(root: &mmu::Root<mmu::L4>) {
    let idt = unsafe { &mut *(&raw mut IDT) };

    idt.set_handler(idt::nr::DOUBLE_FAULT, double_fault as _);
    idt.set_handler(IRQ_TIMER, timer_handler as _);
    for i in IRQ_STUB_OFFSET..=u8::MAX {
        unsafe {
            let p = &irq_stub_table as *const [u8; 5];
            let p = p.add(usize::from(i - IRQ_STUB_OFFSET));
            idt.set_handler(i, p.cast());
        }
    }

    let idtp = (&raw const IDT).addr() as u64;
    let idtp = mmu::Virt::new(idtp).expect("IDT should be in valid virtual space");
    let idtp = root.translate(&page::IdentityMapper, idtp).expect("IDT should be mapped");
    unsafe { IdtPointer::new::<IDT_NR>(idtp).activate() };
}

#[inline(always)]
fn init_apic(root: &mmu::Root<mmu::L4>) {
    let apic = apic::local_address();
    let apic = unsafe { page::phys_to_virt(page::Phys(apic.into())) };
    let apic = unsafe { apic.cast::<apic::local::LocalApic>().as_ref() };
    let apic = LocalApicHelper::new(apic);
    let apic = unsafe { (&mut *(&raw mut LOCAL_APIC)).write(apic) };
    apic.enable();

    let io = {
        let io = apic::io::DEFAULT_ADDR;
        let io = unsafe { page::phys_to_virt(page::Phys(io.into())) };
        let io = unsafe { io.cast::<apic::io::IoApic>().as_ref() };
        let io = IoApicHelper::new(io);
        io
    };

    // FIXME no hardcoding!
    unsafe { io.set_irq(1, 0, 33, TriggerMode::Level, false) }
}

extern "sysv64" fn double_fault() {
    panic!("Double fault!");
}

extern "sysv64" fn irq_handler(id: u8) {
    todo!("irq handler {id}");
}

extern "sysv64" fn timer_handler() {
    todo!("timer handler");
}

unsafe extern "sysv64" {
    static irq_stub_table: [[u8; 5]; 256 - IRQ_STUB_OFFSET as usize];
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
    ".rept 256 - {IRQ_STUB_OFFSET}",
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
    // account for IRQ_STUB_OFFSET while at it
	"lea rcx, [rip + irq_stub_table + 5 - ({IRQ_STUB_OFFSET} * 5)]",
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
    IRQ_STUB_OFFSET = const IRQ_STUB_OFFSET,
    irq_handler = sym irq_handler,
}
