use core::{num::NonZero, ptr::NonNull};

#[macro_export]
macro_rules! door {
    ([$idl:ident $table:ident $name:literal] $($fn:ident)*) => {
        pub fn register() {
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
            unsafe { $crate::door::register(T::ID, $name, table) };
        }
    };
}

const MAX_ENTRIES: usize = 64;
const MAX_NAME_LEN: usize = 64;

static mut API_IDS: [Option<ApiId>; MAX_ENTRIES] = [None; MAX_ENTRIES];
static mut NAMES: [Name; MAX_ENTRIES] = [Name(0); MAX_ENTRIES];
static mut STRINGS: [u8; MAX_ENTRIES * MAX_NAME_LEN] = [0; MAX_ENTRIES * MAX_NAME_LEN];
static mut STRINGS_HEAD: usize = 0;
static mut TABLES: [Option<Table>; MAX_ENTRIES] = [None; MAX_NAME_LEN];
static mut COUNT: usize = 0;

pub struct Interface<'a> {
    pub api: ApiId,
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

#[derive(Clone, Copy)]
struct Name(u32);

unsafe impl Sync for Table {}

impl Name {
    fn as_str(&self) -> &str {
        #[allow(static_mut_refs)]
        unsafe {
            let s = STRINGS.get_unchecked(self.offset()..);
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
            if api.is_some_and(|x| API_IDS[i] != Some(x)) {
                continue;
            }
            return Some((Cookie(id + 1), Interface {
                api: API_IDS[i].unwrap(),
                name: NAMES[i].as_str(),
                table: TABLES[i].unwrap(),
            }));
        }
    }
    None
}

/// # Safety
///
/// The implementation must conform to the API.
pub unsafe fn register(api: ApiId, name: &str, table: Table) {
    let name = alloc_name(name);
    unsafe {
        API_IDS[COUNT] = Some(api);
        NAMES[COUNT] = name;
        TABLES[COUNT] = Some(table);
        COUNT += 1;
    }
}

fn alloc_name(name: &str) -> Name {
    assert!((1..=64).contains(&name.len()), "name must be between 1 and 64 bytes");
    #[allow(static_mut_refs)]
    unsafe {
        STRINGS[STRINGS_HEAD..][..name.len()].copy_from_slice(name.as_bytes());
        let name = Name((STRINGS_HEAD << 6 | (name.len() - 1)) as u32);
        STRINGS_HEAD += name.len();
        name
    }
}
