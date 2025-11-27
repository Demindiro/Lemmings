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
                [$table $name]
                $($fn = $fn,)*
            };
            const _: () = assert!($name.len() >= 1);
            const _: () = assert!($name.len() <= 64);
            let table = const {
                $crate::door::Table(core::ptr::NonNull::new(&T as *const T as *mut T).unwrap().cast())
            };
            unsafe { $crate::door::register(table).expect("enough room in early boot") };
        }
    };
}

const MAX_ENTRIES: usize = 64;
const MAX_NAME_LEN: usize = 64;

static mut TABLES: [Option<Table>; MAX_ENTRIES] = [None; MAX_NAME_LEN];
static mut COUNT: usize = 0;

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

unsafe impl Sync for Table {}

impl Table {
    fn id(&self) -> ApiId {
        unsafe { self.0.cast::<ApiId>().read() }
    }

    fn name(&self) -> &'static str {
        unsafe { self.0.cast::<ApiId>().add(1).cast::<&str>().read() }
    }
}

pub fn list(cookie: Cookie) -> Option<(Cookie, Table)> {
    unsafe {
        for id in cookie.0..COUNT as u64 {
            let i = id as usize;
            let Some(table) = TABLES[i] else { continue };
            return Some((Cookie(id + 1), table));
        }
    }
    None
}

pub fn find(api: ApiId) -> Option<Table> {
    unsafe {
        let tbl = &*(&raw const TABLES);
        (0..COUNT).flat_map(|i| tbl[i]).find(|x| x.id() == api)
    }
}

/// # Safety
///
/// The implementation must conform to the API.
pub unsafe fn register(table: Table) -> Result<(), RegisterError> {
    log!(
        "registering door {:032x} @ {:?} {:?}",
        table.id(),
        table.0,
        table.name(),
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
    unsafe {
        TABLES[n] = Some(table);
    }
    unsafe { COUNT = n + 1 };
    Ok(())
}
