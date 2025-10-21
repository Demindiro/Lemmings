use crate::{KernelEntryToken, page};

pub mod door {
    door! {
        [0x5238e0fc_4d60503d_7b357037_d5319ae5 "boot archive"]
        0 root
    }

    unsafe extern "sysv64" fn root() {
        todo!()
    }

    unsafe extern "sysv64" fn entry() {
        todo!();
    }
}

static mut BASE: &'static [u8] = &[];

const MAGIC: &[u8; 16] = b"Lemmings archive";

pub struct Dir {
    offset: u32,
}

pub struct File {
    offset: u32,
    len: u32,
}

pub enum Item {
    Dir(Dir),
    File(File),
}

struct DirRef {
    offset: u32,
    len: u32,
}

impl Dir {
    pub fn get(&self, name: &str) -> Option<Item> {
        let l = self.len();
        let regions_end = self.offset() + 4 + (4*2*l);
        let mut strings = self.strings();
        (0..l).find_map(|i| {
            let (item_name, len_ty);
            (len_ty, strings) = strings.split_first().expect("truncated");
            let len = 1 + usize::from(len_ty & 0x7f);
            let is_dir = len_ty & 0x80 != 0;
            (item_name, strings) = strings.split_at(len);
            (item_name == name.as_bytes()).then_some((i, is_dir))
        }).map(|(i, is_dir)| {
            let DirRef { offset, len } = self.refs()
                .skip(i)
                .next()
                .expect("#refs == #strings");
            match is_dir {
                true => Item::Dir(Dir { offset }),
                false => Item::File(File { offset, len }),
            }
        })
    }

    pub fn len(&self) -> usize {
        u32_to_usize(u32(self.offset()))
    }

    fn refs(&self) -> impl Iterator<Item = DirRef> {
        base()[self.offset() + 4..self.refs_end()]
            .chunks_exact(8)
            .map(|x| {
                let &[a, b, c, d, ref x @ ..] = x else { unreachable!() };
                let offset = u32::from_le_bytes([a, b, c, d]);
                let &[a, b, c, d] = x else { unreachable!() };
                let len = u32::from_le_bytes([a, b, c, d]);
                DirRef { offset, len }
            })
    }

    fn strings(&self) -> &[u8] {
        &base()[self.refs_end()..]
    }

    fn refs_end(&self) -> usize {
        self.offset() + 4 + (4*2*self.len())
    }

    fn offset(&self) -> usize {
        u32_to_usize(self.offset)
    }
}

impl File {
    pub fn data(&self) -> &'static [u8] {
        segment(u32_to_usize(self.offset), u32_to_usize(self.len))
    }
}

impl core::ops::Deref for File {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.data()
    }
}

impl Item {
    pub fn as_dir(self) -> Option<Dir> {
        match self {
            Self::Dir(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_file(self) -> Option<File> {
        match self {
            Self::File(x) => Some(x),
            _ => None,
        }
    }
}

pub fn init(entry: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    unsafe {
        let start = page::phys_to_virt(entry.data.start).cast::<u8>();
        let end = page::phys_to_virt(entry.data.end).cast::<u8>();
        let len = end.offset_from(start) as usize;
        BASE = core::slice::from_raw_parts(start.as_ptr(), len);
    }
    assert!(base().starts_with(MAGIC), "archive file has corrupt magic");
    token
}

pub fn root() -> Dir {
    Dir { offset: 16 }
}

fn base() -> &'static [u8] {
    unsafe { BASE }
}

#[track_caller]
#[inline(always)]
fn segment(offset: usize, len: usize) -> &'static [u8] {
    &base()[offset..offset + len]
}

#[track_caller]
#[inline(always)]
fn u32(offset: usize) -> u32 {
    u32::from_le_bytes(segment(offset, 4).try_into().expect("4 bytes"))
}

#[track_caller]
#[inline(always)]
fn u32_to_usize(n: u32) -> usize {
    n.try_into().expect("u32 smaller than or equal to usize")
}
