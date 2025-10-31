use crate::{KernelEntryToken, page};

pub mod door {
    use core::ptr::NonNull;
    use lemmings_idl_archive::*;

    door! {
        [lemmings_idl_archive Archive "boot archive"]
        root
        dir_iter
        dir_find
        file_read
    }

    fn to_dir(dir: super::Dir) -> Dir {
        Dir::from(super::merge_u64([dir.offset, 0]))
    }
    fn from_dir(dir: Dir) -> super::Dir {
        let [offset, _] = super::split_u64(dir.into());
        super::Dir { offset }
    }
    fn to_file(file: super::File) -> File {
        File::from(super::merge_u64([file.offset, file.len]))
    }
    fn from_file(file: File) -> super::File {
        let [offset, len] = super::split_u64(file.into());
        super::File { offset, len }
    }

    fn root(_: Root) -> Dir {
        to_dir(super::root())
    }

    unsafe fn dir_iter(x: DirIter) -> FindResult {
        let DirIter { dir, mut cookie, name } = x;
        let cookie = unsafe { cookie.0.as_mut() };
        let Some((c, (n, item))) = from_dir(dir).iter_next(cookie.clone().into()) else {
            return FindResult::ItemNone(ItemNone {
                r#type: NotFound::default(),
                stub: Stub::from(0),
            })
        };
        *cookie = Cookie::from(c);
        let n = NonNull::from(n.as_bytes());
        unsafe { name.base.0.copy_from_nonoverlapping(n.cast(), n.len()) }
        match item {
            super::Item::Dir(dir) => FindResult::ItemDir(ItemDir {
                r#type: IsDir::default(),
                dir: to_dir(dir),
            }),
            super::Item::File(file) => FindResult::ItemFile(ItemFile {
                r#type: IsFile::default(),
                file: to_file(file),
            }),
        }
    }

    unsafe fn dir_find(x: DirFind) -> FindResult {
        let DirFind { dir, name } = x;
        let name = unsafe { core::slice::from_raw_parts(name.base.0.cast().as_ptr(), name.len.into()) };
        let name = unsafe { core::str::from_utf8_unchecked(name) };
        //let name = unsafe { name.as_str() };
        match from_dir(dir).get(name) {
            None => FindResult::ItemNone(ItemNone {
                r#type: NotFound::default(),
                stub: Stub::from(0),
            }),
            Some(super::Item::Dir(dir)) => FindResult::ItemDir(ItemDir {
                r#type: IsDir::default(),
                dir: to_dir(dir),
            }),
            Some(super::Item::File(file)) => FindResult::ItemFile(ItemFile {
                r#type: IsFile::default(),
                file: to_file(file),
            }),
        }
    }

    unsafe fn file_read(x: FileRead) -> ReadResult {
        let FileRead { file, offset, out } = x;
        let file = from_file(file);
        let data = file.data().get(u64::from(offset).try_into().expect("u64 == usize")..).unwrap_or(&[]);
        if data.is_empty() {
            return ReadResult {
                bytes_to_end: FileLen::from(0),
                out_base: out.base,
            }
        }
        unsafe {
            let n = usize::from(out.len).min(data.len());
            let src = NonNull::from(&data[..n]);
            out.base.0.copy_from_nonoverlapping(src.cast(), src.len());
        }
        ReadResult {
            bytes_to_end: FileLen::from(u64::try_from(data.len()).expect("u64 == usize")),
            out_base: out.base,
        }
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
        let name = &strings[..usize::from(namelen)];
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
