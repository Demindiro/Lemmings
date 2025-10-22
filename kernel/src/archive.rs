use crate::{KernelEntryToken, page};

pub mod door {
    use crate::ffi::{Slice, Tuple2};
    use core::{num::NonZero, mem::MaybeUninit, ptr::NonNull};

    door! {
        [0x5238e0fc_4d60503d_7b357037_d5319ae5 "boot archive"]
        0 root
        1 dir_iter
        2 dir_find
        3 file_read
    }

    #[derive(Clone, Copy)]
    #[repr(transparent)]
    struct Cookie(u64);

    #[derive(Clone, Copy)]
    #[repr(transparent)]
    struct Dir(u64);

    #[derive(Clone, Copy)]
    #[repr(transparent)]
    struct File(u64);

    #[derive(Clone, Copy)]
    #[repr(C)]
    union Item {
        dir: Dir,
        file: File,
    }

    #[repr(isize)]
    enum FindState {
        NotFound = -1,
        IsFile = 0,
        IsDir = 1,
    }

    type FindResult = Tuple2<FindState, MaybeUninit<Item>>;

    fn to_dir(dir: super::Dir) -> Dir {
        Dir(super::merge_u64([dir.offset, 0]))
    }
    fn from_dir(dir: Dir) -> super::Dir {
        let [offset, _] = super::split_u64(dir.0);
        super::Dir { offset }
    }
    fn to_file(file: super::File) -> File {
        File(super::merge_u64([file.offset, file.len]))
    }
    fn from_file(file: File) -> super::File {
        let [offset, len] = super::split_u64(file.0);
        super::File { offset, len }
    }

    unsafe extern "sysv64" fn root() -> Dir {
        to_dir(super::root())
    }

    unsafe extern "sysv64" fn dir_iter(dir: Dir, cookie: &mut Cookie, mut name_out: Slice<u8>) -> FindResult {
        let Some((c, (name, item))) = from_dir(dir).iter_next(cookie.0) else {
            return Tuple2(FindState::NotFound, MaybeUninit::uninit());
        };
        *cookie = Cookie(c);
        unsafe { name_out.copy_from_slice(name.as_bytes()) }
        let (state, item) = match item {
            super::Item::Dir(dir) => (FindState::IsDir, Item { dir: to_dir(dir) }),
            super::Item::File(file) => (FindState::IsFile, Item { file: to_file(file) }),
        };
        Tuple2(state, MaybeUninit::new(item))
    }

    unsafe extern "sysv64" fn dir_find(dir: Dir, name: Slice<u8>) -> FindResult {
        let name = unsafe { name.as_str() };
        let (state, item) = match from_dir(dir).get(name) {
            None => (FindState::NotFound, None),
            Some(super::Item::Dir(dir)) => (FindState::IsDir, Some(Item { dir: to_dir(dir) })),
            Some(super::Item::File(file)) => (FindState::IsFile, Some(Item { file: to_file(file) })),
        };
        Tuple2(state, item.map_or_else(MaybeUninit::uninit, MaybeUninit::new))
    }

    unsafe extern "sysv64" fn file_read(file: File, offset: u64, out: Slice<u8>) -> Tuple2<u64, NonNull<u8>> {
        let file = from_file(file);
        let data = file.data().get(offset.try_into().expect("u64 == usize")..).unwrap_or(&[]);
        if data.is_empty() {
            return Tuple2(0, out.into());
        }
        unsafe {
            let n = out.len().min(data.len());
            out.subslice(0..n).copy_from_slice(&data[..n]);
        }
        let len = data.len().try_into().expect("u64 == usize");
        Tuple2(len, out.into())
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
        self.iter().find(|&(n, _)| n == name).map(|(_, x)| x)
    }

    fn iter(&self) -> impl Iterator<Item = (&'static str, Item)> + '_ {
        let mut cookie = 0;
        core::iter::from_fn(move || {
            let x;
            (cookie, x) = self.iter_next(cookie)?;
            Some(x)
        })
    }

    fn iter_next(&self, cookie: u64) -> Option<(u64, (&'static str, Item))> {
        let [i, name_i] = split_u64(cookie);
        if i >= self.len32() {
            return None;
        }
        let strings = &self.strings()[u32_to_usize(name_i)..];
        let (len_ty, strings) = strings.split_first().expect("truncated");
        let namelen = 1 + (len_ty & 0x7f);
        let is_dir = len_ty & 0x80 != 0;
        let (name, strings) = strings.split_at(usize::from(namelen));
        let name = core::str::from_utf8(name).expect("name is not UTF-8");
        let DirRef { offset, len } = self.refs()
            .skip(u32_to_usize(i))
            .next()
            .expect("#refs == #strings");
        let item = match is_dir {
            true => Item::Dir(Dir { offset }),
            false => Item::File(File { offset, len }),
        };
        let (ni, name_ni) = (i + 1, name_i + 1 + u32::from(namelen));
        Some((merge_u64([ni, name_ni]), (name, item)))
    }

    pub fn len(&self) -> usize {
        u32_to_usize(self.len32())
    }

    pub fn len32(&self) -> u32 {
        u32(self.offset())
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

    fn strings(&self) -> &'static [u8] {
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

#[inline(always)]
fn split_u64(x: u64) -> [u32; 2] {
    [x, x >> 32].map(|x| x as u32)
}

#[inline(always)]
fn merge_u64([x, y]: [u32; 2]) -> u64 {
    u64::from(y) << 32 | u64::from(x)
}
