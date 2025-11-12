mod x86_64;

pub use x86_64::{Msi, register_msi, unregister_msi};

use crate::KernelEntryToken;

pub mod door {
    pub fn register() {
        super::x86_64::door::register_door();
    }
}

pub fn init(entry: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    let _ = entry;
    let token = x86_64::init(token);
    token
}
