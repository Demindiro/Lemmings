// To avoid strange errors, we should ensure we have an immutable view of the archive folder.
//
// The most practical way to guarantee is to make a private memory mapping of all files.
// This does mean we'll need to build our own index, but that isn't necessarily a bad thing
// as it avoids extra system calls and allows us to make sure it is sorted.
// (we also get to avoid read() calls, yay!)

use crate::{
    linux::{CStr, LinuxDirent64, Stat},
    syscall,
};
use core::{
    fmt,
    marker::PhantomData,
    mem::{self, MaybeUninit},
    ptr::{self, NonNull},
    slice, str,
};

static mut ROOT: MaybeUninit<Dir> = MaybeUninit::uninit();

#[derive(Clone, Copy)]
pub enum Item {
    File(File),
    Dir(Dir),
}

#[derive(Clone, Copy)]
pub struct Dir {
    base: NonNull<u8>,
}

#[derive(Clone, Copy)]
pub struct File {
    data: &'static [u8],
}

#[derive(Debug)]
struct RawDirEntry {
    child_buf: NonNull<u8>,
    child_len: usize,
    name_offset: u32,
    name_len: u16,
    is_dir: bool,
}

#[derive(Debug)]
struct DirEntry<'a> {
    child_buf: NonNull<u8>,
    child_len: usize,
    is_dir: bool,
    name: &'a str,
}

struct IterDir<'a> {
    cur: NonNull<u8>,
    end: NonNull<u8>,
    _marker: PhantomData<&'a [MaybeUninit<usize>]>,
}

impl Dir {
    pub fn find(&self, name: &str) -> Option<Item> {
        self.entries()
            .find(|x| x.name == name)
            .map(|x| match x.is_dir {
                true => Item::Dir(Dir { base: x.child_buf }),
                false => Item::File(File {
                    data: unsafe { slice::from_raw_parts(x.child_buf.as_ptr(), x.child_len) },
                }),
            })
    }

    fn entries(&self) -> impl Iterator<Item = DirEntry<'_>> + '_ {
        let (entries, names) = self.raw_entries();
        entries.into_iter().map(move |e| {
            let name = unsafe { e.name(names) };
            let name = str::from_utf8(name).expect("UTF-8");
            DirEntry {
                child_buf: e.child_buf,
                child_len: e.child_len,
                is_dir: e.is_dir,
                name,
            }
        })
    }

    fn raw_entries(&self) -> (&[RawDirEntry], NonNull<u8>) {
        let (num, base) = self.len_entries();
        let entries = unsafe { slice::from_raw_parts(base.as_ptr(), num) };
        let names = unsafe { base.cast::<RawDirEntry>().add(num).cast() };
        (entries, names)
    }

    fn raw_entries_mut(&mut self) -> (&mut [RawDirEntry], NonNull<u8>) {
        let (num, base) = self.len_entries();
        let entries = unsafe { slice::from_raw_parts_mut(base.as_ptr(), num) };
        let names = unsafe { base.cast::<RawDirEntry>().add(num).cast() };
        (entries, names)
    }

    fn len_entries(&self) -> (usize, NonNull<RawDirEntry>) {
        let num = unsafe { self.base.cast::<usize>().read() };
        let base = unsafe { self.base.cast::<usize>().add(1) };
        (num, base.cast())
    }
}

impl RawDirEntry {
    unsafe fn name(&self, names: NonNull<u8>) -> &[u8] {
        unsafe {
            let name = names.add(self.name_offset as usize).as_ptr();
            slice::from_raw_parts(name, self.name_len.into())
        }
    }
}

impl File {
    pub fn data(&self) -> &'static [u8] {
        self.data
    }
}

impl<'a> Iterator for IterDir<'a> {
    type Item = &'a LinuxDirent64;

    fn next(&mut self) -> Option<Self::Item> {
        while self.cur < self.end {
            let entry = unsafe { self.cur.cast::<LinuxDirent64>().as_ref() };
            self.cur = unsafe { self.cur.byte_add(entry.record_len()) };
            if entry
                .name()
                .is_some_and(|x| [&b"."[..], b".."].contains(&x.as_bytes()))
            {
                continue;
            }
            return Some(entry);
        }
        None
    }
}

impl fmt::Debug for Dir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.entries()).finish()
    }
}

pub fn root() -> Dir {
    unsafe { ROOT.assume_init() }
}

pub unsafe fn init() {
    let dirfd = unsafe { syscall::open(".\0".as_ptr(), syscall::O_RDONLY, 0) };
    assert!(dirfd >= 0, "failed to open root at \".\" (error {dirfd})");
    let dirent = &mut [const { MaybeUninit::uninit() }; 1024];
    let root = unsafe { collect_dir(dirfd, dirent) };
    unsafe { syscall::close(dirfd) };
    unsafe {
        (&mut *(&raw mut ROOT)).write(root);
    }
}

unsafe fn collect_dir<const N: usize>(dirfd: i32, dirent: &mut [MaybeUninit<usize>; N]) -> Dir {
    let num_bytes = unsafe { syscall::getdents64(dirfd, dirent.as_mut_ptr().cast(), dirent.len()) };
    assert!(
        num_bytes >= 0,
        "failed to iterate directory (error {num_bytes})"
    );
    let cur = NonNull::from(&mut *dirent).cast();
    let end = unsafe { cur.byte_add(num_bytes as usize).cast() };
    let iter = || IterDir {
        cur,
        end,
        _marker: PhantomData,
    };
    for entry in iter() {}
    let entry_count = iter().count();
    let names_len = iter().fold(0, |s, x| s + 1 + x.name().expect("valid CStr").len());
    let buf_size = 8 + 8 * entry_count + names_len;
    let buf = alloc(buf_size);
    // TODO sort entries
    unsafe {
        buf.cast::<usize>().write(entry_count);
        let mut p = buf.byte_add(mem::size_of::<usize>());
        let mut name_offset = 0;
        for x in iter() {
            let name = x.name().expect("valid CStr");
            let name_len = u16::try_from(name.len()).expect("name too long");
            let is_dir = match x.ty() {
                LinuxDirent64::TY_FILE => false,
                LinuxDirent64::TY_DIR => true,
                ty => panic!("unsupported file type {ty}"),
            };
            p.cast::<RawDirEntry>().write(RawDirEntry {
                child_buf: NonNull::dangling(),
                child_len: 0,
                name_offset,
                name_len,
                is_dir,
            });
            name_offset += u32::from(name_len);
            p = p.byte_add(mem::size_of::<RawDirEntry>());
        }
        for x in iter() {
            let name = x.name().expect("valid CStr");
            p.as_ptr()
                .copy_from_nonoverlapping(name.as_ptr(), name.len());
            p = p.byte_add(name.len());
        }
    }
    let mut dir = Dir { base: buf };

    let (entries, names) = dir.raw_entries_mut();
    for x in entries {
        const { assert!(N * mem::size_of::<usize>() > 256) };
        let mut name = NonNull::from(&mut *dirent).cast::<u8>();
        let xname = unsafe { x.name(names) };
        unsafe {
            name.copy_from_nonoverlapping(NonNull::from(xname).cast(), xname.len());
            name.byte_add(xname.len()).write(0);
        }
        let fd = unsafe { syscall::openat(dirfd, name.as_ptr(), syscall::O_RDONLY, 0) };
        assert!(fd >= 0, "failed to open entry (error {fd})");
        (x.child_buf, x.child_len) = if x.is_dir {
            let Dir { base } = unsafe { collect_dir(fd, dirent) };
            (base, 0)
        } else {
            let s = unsafe { map_file(fd) };
            (NonNull::from(s).cast(), s.len())
        };
        unsafe { syscall::close(fd) };
    }

    dir
}

fn map_file(fd: i32) -> &'static [u8] {
    let stat = &mut Stat::default();
    let res = unsafe { syscall::fstat(fd, stat) };
    assert!(res >= 0, "failed to stat fd (error {res})");
    let len = usize::try_from(stat.size).expect("file too large to memory map");
    let ptr = memmap(len, fd, false).expect("file memory map failed");
    unsafe { slice::from_raw_parts(ptr.as_ptr(), len) }
}

fn alloc(len: usize) -> NonNull<u8> {
    memmap(len, -1, true).expect("allocation failed")
}

fn memmap(len: usize, fd: i32, write: bool) -> Result<NonNull<u8>, i32> {
    let prot = if write {
        syscall::PROT_RW
    } else {
        syscall::PROT_R
    };
    let flags = if fd < 0 { syscall::MAP_ANON } else { 0 } | syscall::MAP_PRIVATE;
    let ptr = unsafe { syscall::mmap(ptr::null_mut(), len, prot, flags, fd, 0) };
    match ptr as isize {
        x @ -4095..0 => Err(x as i32),
        _ => NonNull::new(ptr).ok_or(0),
    }
}
