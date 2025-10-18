use crate::io::{self, Const};

#[allow(dead_code)]
const CH0: Const<0x40> = Const;
#[allow(dead_code)]
const CH1: Const<0x41> = Const;
const CH2: Const<0x42> = Const;
const CMD: Const<0x43> = Const;
const SPEAKER: Const<0x61> = Const;

const FREQ_HZ: u32 = 1193180;

pub unsafe fn countdown_start(dt_ns: u64) {
    // https://wiki.osdev.org/APIC_Timer#Example_code_in_ASM
    unsafe {
        let v = io::in_::<u8, _>(SPEAKER);
        io::out(SPEAKER, (v & 0xfd) | 1);
        io::out(CMD, 0b10_11_001_0_u8); // CH2, lo+hibyte, oneshot, 16-bit binary
        let [l, h] = ((dt_ns * u64::from(FREQ_HZ) / 1_000_000_000) as u16).to_le_bytes();
        io::out(CH2, l);
        io::in_::<u8, _>(Const::<0x60>); // TODO ??? short delay ???
        io::out(CH2, h);
        // reset PIT one-shot counter (start counting)
        let v = io::in_::<u8, _>(SPEAKER);
        io::out(SPEAKER, v & 0xfe);
        io::out(SPEAKER, v | 1);
    }
}

pub unsafe fn countdown_finished() -> bool {
    unsafe { io::in_::<u8, _>(SPEAKER) & 0x20_u8 != 0 }
}
