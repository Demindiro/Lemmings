use crate::KernelEntryToken;
use core::{mem::MaybeUninit, ptr::NonNull};
use lemmings_tty::Tty;

static mut TTY: MaybeUninit<Tty<32>> = MaybeUninit::uninit();
const MAX_WIDTH: u16 = 720;
const MAX_HEIGHT: u16 = 600;
const MAX_CHARS: usize = (MAX_WIDTH as usize / 6) * (MAX_HEIGHT as usize / 12);
static mut CHARS: [char; MAX_CHARS] = ['\0'; MAX_CHARS];

#[cfg(feature = "TODO")]
pub mod door {
    use core::ptr::NonNull;

    door! {
        [0xd8112085_698f85f2_dceefb6d_4758a59f "boot framebuffer"]
        0 acquire
        1 release
        2 inuse
    }

    #[repr(C)]
    struct FrameBuffer {
        base: Option<NonNull<u8>>,
        width: u16,
        height: u16,
        stride: u16,
        format: u8,
    }

    unsafe extern "sysv64" fn acquire(disable_callback: extern "sysv64" fn()) -> FrameBuffer {
        let _ = disable_callback;
        FrameBuffer {
            base: Some(const { NonNull::new(0xc000_0000u32 as _).unwrap() }),
            width: 1024,
            height: 768,
            stride: 1024,
            format: 0,
        }
    }

    unsafe extern "sysv64" fn release() {
    }

    unsafe extern "sysv64" fn inuse() -> bool {
        todo!()
    }
}

pub fn init(entry: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    use lemmings_qemubios::ColorFormat;
    let fb = &entry.framebuffer;
    match fb.format {
        ColorFormat::None => return token,
        ColorFormat::Rgbx8888 => {}
        ColorFormat::Bgrx8888 => {}
    }
    let base = NonNull::new(fb.base.0 as *mut u32).unwrap();
    let [w, h, s] = [fb.width, fb.height, fb.stride].map(u32::from);
    dbg!(w, h, s);
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

    let tty = init_tty(fb);
    for i in 0u64.. {
        use core::fmt::Write;
        write!(tty, "Hello framebuffer! (nr: {i})\n");
        tty.flush();
    }

    token
}

fn init_tty<'a>(fb: &'a lemmings_qemubios::FrameBuffer) -> &'static mut Tty<'static, 32> {
    let base = NonNull::new(fb.base.0 as *mut u32).unwrap();
    let tty = unsafe { &mut *(&raw mut TTY) };
    let chars = unsafe { &mut *(&raw mut CHARS) };
    let tty = tty.write(Tty::new(base.cast(), fb.width.min(MAX_WIDTH), fb.height.min(MAX_HEIGHT), fb.stride, chars));
    tty
}
