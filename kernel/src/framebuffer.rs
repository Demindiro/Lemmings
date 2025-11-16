use crate::{
    KernelEntryToken,
    sync::{SpinLock, SpinLockGuard},
};
use core::{fmt, mem::MaybeUninit, ptr::NonNull};
use critical_section::CriticalSection;
use lemmings_tty::Tty;

static mut TTY: MaybeUninit<SpinLock<Tty<32>>> = MaybeUninit::uninit();
const MAX_WIDTH: u16 = 720;
const MAX_HEIGHT: u16 = 600;
const MAX_CHARS: usize = (MAX_WIDTH as usize / 6) * (MAX_HEIGHT as usize / 12);
static mut CHARS: [char; MAX_CHARS] = ['\0'; MAX_CHARS];

pub struct Log<'a, 'cs> {
    tty: SpinLockGuard<'a, 'cs, Tty<'a, 32>>,
}

impl fmt::Write for Log<'_, '_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        // exclude \r
        s.split('\r')
            .try_for_each(|s| fmt::Write::write_str(&mut *self.tty, s))
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        if c == '\r' {
            return Ok(());
        }
        fmt::Write::write_char(&mut *self.tty, c)
    }
}

impl Drop for Log<'_, '_> {
    fn drop(&mut self) {
        self.tty.flush();
    }
}

pub fn log<'cs>(cs: CriticalSection<'cs>) -> Log<'static, 'cs> {
    let tty = unsafe { (&*(&raw const TTY)).assume_init_ref() };
    let tty = tty.lock(cs);
    Log { tty }
}

pub fn init<'a>(token: KernelEntryToken<'a>) -> KernelEntryToken<'a> {
    use lemmings_qemubios::ColorFormat;
    let fb = &token.framebuffer;
    match fb.format {
        ColorFormat::None => return token,
        ColorFormat::Rgbx8888 => {}
        ColorFormat::Bgrx8888 => {}
    }
    let base = NonNull::new(fb.base.0 as *mut u32).unwrap();
    let [w, h, s] = [fb.width, fb.height, fb.stride].map(u32::from);
    for y in 0..h {
        for x in 0..w {
            let b = 256 * x / w;
            let g = 256 * y / h;
            let r = (511 - (b + g)) / 2;
            let pix = r << 16 | g << 8 | b;
            unsafe {
                base.byte_add((y * s) as usize).add(x as usize).write(pix);
            }
        }
    }

    init_tty(fb);

    token
}

fn init_tty(fb: &lemmings_qemubios::FrameBuffer) {
    let base = NonNull::new(fb.base.0 as *mut u32).unwrap();
    let tty = unsafe { &mut *(&raw mut TTY) };
    let chars = unsafe { &mut *(&raw mut CHARS) };
    tty.write(SpinLock::new(Tty::new(
        base.cast(),
        fb.width.min(MAX_WIDTH),
        fb.height.min(MAX_HEIGHT),
        fb.stride,
        chars,
    )));
}
