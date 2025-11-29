use core::arch::asm;
use critical_section::RawRestoreState;

struct Impl;
critical_section::set_impl!(Impl);

unsafe impl critical_section::Impl for Impl {
    unsafe fn acquire() -> RawRestoreState {
        let token;
        unsafe {
            asm! {
                "pushf",
                "pop {0:r}",
                "cli",
                out(reg) token,
                options(nomem, preserves_flags),
            }
        }
        token
    }

    unsafe fn release(token: RawRestoreState) {
        unsafe {
            asm! {
                "push {0:r}",
                "popf",
                in(reg) token,
                options(nomem),
            }
        }
    }
}
