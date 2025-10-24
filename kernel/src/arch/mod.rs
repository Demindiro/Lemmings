mod x86_64;

use crate::KernelEntryToken;

pub fn init(entry: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    let _ = entry;
    let token = x86_64::init(token);
    token
}
