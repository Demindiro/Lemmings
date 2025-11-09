#![allow(dead_code)]

#[derive(Default)]
pub struct Monotonic(u64);

impl Monotonic {
    pub fn nanos(&self) -> u64 {
        self.0
    }
}
