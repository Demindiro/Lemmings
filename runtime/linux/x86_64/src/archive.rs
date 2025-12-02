// To avoid strange errors, we should ensure we have an immutable view of the archive folder.
//
// The most practical way to guarantee is to make a private memory mapping of all files.
// This does mean we'll need to build our own index, but that isn't necessarily a bad thing
// as it avoids extra system calls and allows us to make sure it is sorted.
// (we also get to avoid read() calls, yay!)

use crate::{
    linux::{CStr, LinuxDirent64},
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

struct Dir {
    base: NonNull<u8>,
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
    fn entries(&self) -> impl Iterator<Item = DirEntry<'_>> + '_ {
        let (entries, names) = self.raw_entries();
        entries.into_iter().map(move |e| unsafe {
            let name = unsafe { names.add(e.name_offset as usize).as_ptr() };
            let name = unsafe { slice::from_raw_parts(name, e.name_len.into()) };
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
        let num = unsafe { self.base.cast::<usize>().read() };
        let base = unsafe { self.base.cast::<usize>().add(1) };
        let entries = unsafe { slice::from_raw_parts(base.cast().as_ptr(), num) };
        let names = unsafe { base.cast::<RawDirEntry>().add(num).cast() };
        (entries, names)
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

pub unsafe fn init() {
    let dirfd = unsafe { syscall::open(".\0".as_ptr(), syscall::O_RDONLY, 0) };
    assert!(dirfd >= 0, "failed to open root at \".\" (error {dirfd})");
    let dirent = &mut [const { MaybeUninit::uninit() }; 1024];
    let root = unsafe { collect_dir(dirfd, dirent) };
    unsafe { syscall::close(dirfd) };
    dbg!(&root);
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
    let dir = Dir { base: buf };

    for x in dir.entries() {
        const { assert!(N * mem::size_of::<usize>() > 128) };
        let mut name = NonNull::from(&mut *dirent).cast::<u8>();
        unsafe {
            name.copy_from_nonoverlapping(NonNull::from(x.name).cast(), x.name.len());
            name.byte_add(x.name.len()).write(0);
        }
        let fd = unsafe { syscall::openat(dirfd, name.as_ptr(), syscall::O_RDONLY, 0) };
        if x.is_dir {
            unsafe { collect_dir(fd, dirent) };
        } else {
            unsafe { map_file(fd) };
        }
        unsafe { syscall::close(fd) };
    }

    dir
}

unsafe fn map_file(fd: i32) -> &'static [u8] {
    todo!()
}

fn alloc(len: usize) -> NonNull<u8> {
    let ptr = unsafe {
        syscall::mmap(
            ptr::null_mut(),
            len,
            syscall::PROT_RW,
            syscall::MAP_ANON | syscall::MAP_PRIVATE,
            0,
            0,
        )
    };
    // FIXME we need to check -4095..0, not just null
    NonNull::new(ptr).expect("failed to allocate memory")
}
