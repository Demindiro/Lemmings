use core::{num::NonZero, ptr::NonNull};

#[macro_export]
macro_rules! door {
    ([$idl:ident $table:ident $name:literal] $($fn:ident)*) => {
        door!(register [$idl $table $name] $($fn)*);
    };
    ($register:ident [$idl:ident $table:ident $name:literal] $($fn:ident)*) => {
        pub fn $register() {
            type T = $idl::$table;
            static T: T = $idl::imp! {
                [$table]
                $($fn = $fn,)*
            };
            const _: () = assert!($name.len() >= 1);
            const _: () = assert!($name.len() <= 64);
            let table = const {
                $crate::door::Table(core::ptr::NonNull::new(&T as *const T as *mut T).unwrap().cast())
            };
            unsafe { $crate::door::register($name, table).expect("enough room in early boot") };
        }
    };
}

const MAX_ENTRIES: usize = 64;
const MAX_NAME_LEN: usize = 64;

static mut NAMES: [Name; MAX_ENTRIES] = [Name(0); MAX_ENTRIES];
static mut STRINGS: [u8; MAX_ENTRIES * MAX_NAME_LEN] = [0; MAX_ENTRIES * MAX_NAME_LEN];
static mut STRINGS_HEAD: usize = 0;
static mut TABLES: [Option<Table>; MAX_ENTRIES] = [None; MAX_NAME_LEN];
static mut COUNT: usize = 0;

pub struct Interface<'a> {
    pub name: &'a str,
    pub table: Table,
}

pub type ApiId = NonZero<u128>;

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Cookie(u64);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Table(pub NonNull<NonNull<()>>);

#[derive(Clone, Debug)]
pub enum RegisterError {
    Duplicate,
    Full,
}

#[derive(Clone, Copy)]
struct Name(u32);

unsafe impl Sync for Table {}

impl Table {
    fn id(&self) -> ApiId {
        unsafe { self.0.cast::<ApiId>().read() }
    }
}

impl Name {
    fn as_str(&self) -> &str {
        unsafe {
            let s = &mut *(&raw mut STRINGS);
            let s = s.get_unchecked(self.offset()..);
            let s = s.get_unchecked(..self.len());
            core::str::from_utf8_unchecked(s)
        }
    }

    fn len(&self) -> usize {
        1 + (self.0 & 63) as usize
    }

    fn offset(&self) -> usize {
        (self.0 >> 6) as usize
    }
}

pub fn list(api: Option<ApiId>, cookie: Cookie) -> Option<(Cookie, Interface<'static>)> {
    unsafe {
        for id in cookie.0..COUNT as u64 {
            let i = id as usize;
            let Some(table) = TABLES[i] else { continue };
            if api.is_some_and(|x| table.id() != x) {
                continue;
            }
            return Some((
                Cookie(id + 1),
                Interface {
                    name: NAMES[i].as_str(),
                    table,
                },
            ));
        }
    }
    None
}

pub fn find(api: ApiId) -> Option<Interface<'static>> {
    list(Some(api), Cookie(0)).map(|x| x.1)
}

/// # Safety
///
/// The implementation must conform to the API.
pub unsafe fn register(name: &str, table: Table) -> Result<(), RegisterError> {
    log!(
        "registering door {:032x} @ {:?} {name:?}",
        table.id(),
        table.0
    );
    if find(table.id()).is_some() {
        log!("door entry duplicate");
        return Err(RegisterError::Duplicate);
    }
    let n = unsafe { COUNT };
    if n >= MAX_ENTRIES {
        log!("door entries full");
        return Err(RegisterError::Full);
    }
    let name = alloc_name(name);
    unsafe {
        NAMES[n] = name;
        TABLES[n] = Some(table);
    }
    unsafe { COUNT = n + 1 };
    Ok(())
}

fn alloc_name(name: &str) -> Name {
    assert!(
        (1..=64).contains(&name.len()),
        "name must be between 1 and 64 bytes"
    );
    unsafe {
        let strings = &mut *(&raw mut STRINGS);
        strings[STRINGS_HEAD..][..name.len()].copy_from_slice(name.as_bytes());
        let name = Name((STRINGS_HEAD << 6 | (name.len() - 1)) as u32);
        STRINGS_HEAD += name.len();
        name
    }
}
