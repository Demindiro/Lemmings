use super::reg::{R, RW, Reg, W};
use core::{fmt, num::NonZeroU64, time::Duration};

const APIC_NMI: u32 = 1 << 4;
const APIC_DISABLE: u32 = 0x1_0000;
const APIC_SW_ENABLE: u32 = 0x100;

#[derive(Debug)]
pub struct LocalApicHelper<'a> {
    apic: &'a LocalApic,
    ticks_per_second: u32,
}

impl<'a> LocalApicHelper<'a> {
    pub fn new(apic: &'a LocalApic) -> Self {
        let s = Self {
            apic,
            ticks_per_second: 0,
        };
        s.init();
        s
    }

    /// Initialize to a well-known state (https://wiki.osdev.org/APIC_timer#Example_code_in_ASM)
    fn init(&self) {
        self.apic.destination_format.set(0xffff_ffff);
        let logical_dest = self.apic.logical_destination.get();
        self.apic
            .logical_destination
            .set((logical_dest & 0xff_ffff) | 1);
        self.apic.lvt_timer.set(APIC_DISABLE);
        self.apic.lvt_performance_monitoring_counters.set(APIC_NMI);
        self.apic.lvt_lint0.set(APIC_DISABLE);
        self.apic.lvt_lint1.set(APIC_DISABLE);
        self.apic.task_priority.set(0);
    }

    /// Set the APIC frequency directly.
    pub fn set_ticks_per_second(&mut self, value: u32) {
        self.ticks_per_second = value;
    }

    /// Loop for the given duration and count the amount of passed ACPI timer cycles to
    /// calibrate the timer.
    pub fn calibrate_timer<F>(&mut self, dt_ns: NonZeroU64, mut poll: F)
    where
        F: FnMut() -> bool,
    {
        self.apic.divide_configuration.set(0b1011); // Set divisor to 1
        self.apic.initial_count.set(u32::MAX);
        while !poll() { /* pass */ }
        let ticks = u32::MAX - self.apic.current_count.get();
        self.apic.initial_count.set(0);
        self.ticks_per_second = u128::from(ticks)
            .saturating_mul(1_000_000_000)
            .saturating_div(dt_ns.get().into())
            .try_into()
            .unwrap_or(u32::MAX)
    }

    /// Set IRQ for timer.
    pub fn set_lvt(&self, irq: u8) {
        let t = self.apic.lvt_timer.get();
        self.apic
            .lvt_timer
            .set((t & !0xff) | (1 << 16) | u32::from(irq));
    }

    /// Enable APIC & map spurious IRQ
    pub fn enable(&self) {
        self.apic
            .spurious_interrupt_vector
            .set(APIC_SW_ENABLE | 0xff);
    }

    pub fn set_timer_initial_count(&self, value: u32) {
        self.apic.initial_count.set(value);
    }

    /// Set the timer in one-shot mode for the given duration in the future.
    ///
    /// Smaller durations are more precise. The timer may end early if the duration
    /// is too large.
    pub fn set_timer_oneshot(&self, t: Duration, irq: u8) {
        let mut ticks = t
            .as_nanos()
            .saturating_mul(u128::from(self.ticks_per_second))
            .saturating_div(1_000_000_000);
        // Scale down the resolution until the ticks fit
        let mut shift = 0;
        let ticks = loop {
            if let Ok(ticks) = ticks.try_into() {
                break ticks;
            }
            ticks >>= 1;
            shift += 1;
        };
        // Translate shift to something we can put in the divide configuration reigster
        let (shift, ticks) = match shift {
            0 => (0b1011, ticks),
            1 => (0b0000, ticks),
            2 => (0b1000, ticks),
            3 => (0b0010, ticks),
            4 => (0b1010, ticks),
            5 => (0b0001, ticks),
            6 => (0b1001, ticks),
            7 => (0b0011, ticks),
            _ => (0b0011, u32::MAX), // Default to highest
        };

        let t = self.apic.lvt_timer.get();
        self.apic
            .lvt_timer
            .set((t & !(1 << 16 | 0xff)) | u32::from(irq));
        self.apic.divide_configuration.set(shift);
        self.apic.initial_count.set(ticks);
    }

    pub fn set_eoi(&self, value: u32) {
        self.apic.eoi.set(value);
    }
}

#[repr(C, align(4096))]
pub struct LocalApic {
    _reserved_0_1: [Reg<R>; 0x1 - 0x0 + 1],
    pub id: Reg<R>,
    pub version: Reg<R>,
    _reserved_4_7: [Reg<R>; 0x7 - 0x4 + 1],
    pub task_priority: Reg<RW>,
    pub arbitration_priority: Reg<R>,

    pub processor_priority: Reg<R>,
    pub eoi: Reg<W>,
    pub remote_read: Reg<R>,
    pub logical_destination: Reg<RW>,
    pub destination_format: Reg<RW>,
    pub spurious_interrupt_vector: Reg<RW>,

    pub in_service: [Reg<R>; 8],
    pub trigger_mode: [Reg<R>; 8],

    pub interrupt_request: [Reg<R>; 8],
    pub error_status: Reg<R>,
    _reserved_29_2e: [Reg<R>; 0x2e - 0x29 + 1],
    pub lvt_cmci: Reg<RW>,

    pub interrupt_command: [Reg<RW>; 2],
    pub lvt_timer: Reg<RW>,
    pub lvt_thermal_sensor: Reg<RW>,
    pub lvt_performance_monitoring_counters: Reg<RW>,
    pub lvt_lint0: Reg<RW>,
    pub lvt_lint1: Reg<RW>,
    pub lvt_error: Reg<RW>,
    pub initial_count: Reg<RW>,
    pub current_count: Reg<R>,
    _reserved_3a_3d: [Reg<R>; 0x3d - 0x3a + 1],
    pub divide_configuration: Reg<RW>,
}

impl LocalApic {
    pub fn in_service(&self) -> BitSet256 {
        reg_to_bitset(&self.in_service)
    }

    pub fn trigger_mode(&self) -> BitSet256 {
        reg_to_bitset(&self.trigger_mode)
    }

    pub fn interrupt_request(&self) -> BitSet256 {
        reg_to_bitset(&self.interrupt_request)
    }
}

fn reg_to_bitset(regs: &[Reg<R>; 8]) -> BitSet256 {
    let mut set = [0; 2];
    // TODO don't use transmute
    let v = unsafe { core::mem::transmute::<_, &mut [u32; 8]>(&mut set) };
    regs.iter()
        .zip(v.iter_mut())
        .for_each(|(r, w)| *w = r.get());
    BitSet256(set)
}

impl fmt::Debug for LocalApic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use core::fmt::DebugStruct as DS;
        let mut f = f.debug_struct(stringify!(LocalApic));
        let hex = |f: &mut DS, n, v: u32| {
            f.field(n, &format_args!("{:#x}", v));
        };
        let set = |f: &mut DS, n, v: BitSet256| {
            f.field(n, &format_args!("{:?}", v));
        };

        hex(&mut f, "id", self.id.get());
        hex(&mut f, "version", self.version.get());
        hex(&mut f, "task_priority", self.task_priority.get());
        hex(
            &mut f,
            "arbitration_priority",
            self.arbitration_priority.get(),
        );
        hex(&mut f, "processor_priority", self.processor_priority.get());
        // FIXME QEMU sets ESR even though it shouldn't
        // See https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-vol-3a-part-1-manual.pdf
        //hex(&mut f, "remote_read",  self.remote_read.get());
        hex(
            &mut f,
            "logical_destination",
            self.logical_destination.get(),
        );
        hex(&mut f, "destination_format", self.destination_format.get());
        hex(
            &mut f,
            "spurious_interrupt_vector",
            self.spurious_interrupt_vector.get(),
        );
        set(&mut f, "in_service", self.in_service());
        set(&mut f, "trigger_mode", self.trigger_mode());
        set(&mut f, "interrupt_request", self.interrupt_request());
        hex(&mut f, "error_status", self.error_status.get());
        // FIXME ditto
        //hex(&mut f, "lvt_cmci",  self.lvt_cmci.get());
        //hex(&mut f, "interrupt_command",  self.interrupt_command.get());
        hex(&mut f, "lvt_timer", self.lvt_timer.get());
        hex(&mut f, "lvt_thermal_sensor", self.lvt_thermal_sensor.get());
        hex(
            &mut f,
            "lvt_performance_monitoring_counters",
            self.lvt_performance_monitoring_counters.get(),
        );
        hex(&mut f, "lvt_lint0", self.lvt_lint0.get());
        hex(&mut f, "lvt_lint1", self.lvt_lint1.get());
        hex(&mut f, "lvt_error", self.lvt_error.get());
        hex(&mut f, "initial_count", self.initial_count.get());
        hex(&mut f, "current_count", self.current_count.get());
        hex(
            &mut f,
            "divide_configuration",
            self.divide_configuration.get(),
        );

        f.finish()
    }
}

pub struct BitSet256([u128; 2]);

impl fmt::Debug for BitSet256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut f = f.debug_set();
        let mut p = |i, v, o| {
            if v & (1u128 << i) > 0u128 {
                let i = i + o;
                f.entry(&i);
            }
        };
        (0..128).for_each(|i| p(i, self.0[0], 0));
        (0..128).for_each(|i| p(i, self.0[1], 128));
        f.finish()
    }
}
