use crate::io;
use core::fmt;

const IO_BASE: u16 = 0x3f8;

pub struct Writer;

pub fn put(byte: u8) {
    unsafe { io::out(IO_BASE, byte) };
}

pub fn write(bytes: &[u8]) {
    bytes.iter().for_each(|&b| put(b));
}

impl fmt::Write for Writer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        write(s.as_bytes());
        Ok(())
    }
}
