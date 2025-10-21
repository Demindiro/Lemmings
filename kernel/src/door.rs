use core::{num::NonZero, ptr::NonNull};

#[macro_export]
macro_rules! door {
    ([$id:literal $name:literal] $($num:literal $fn:ident)*) => {
        const API: $crate::door::ApiId = $crate::door::ApiId::new($id).expect("non-zero");
        static TABLE: [$crate::door::Routine; 1 + door!(+ $($num)*)] = [
            $($crate::door::Routine(core::ptr::NonNull::new($fn as *mut ()).expect("function is non-null")),)*
        ];
        const _: () = assert!($name.len() >= 1);
        const _: () = assert!($name.len() <= 64);
        pub fn register() {
            unsafe { $crate::door::register_internal(API, $name, &TABLE) };
        }
    };
    (+ $x:literal) => { $x };
    (+ $x:literal $($xs:literal)*) => { door!(+ $($xs)*) };
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
pub struct Routine(pub NonNull<()>);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Cookie(u64);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Table(NonNull<NonNull<()>>);

#[derive(Clone, Copy)]
struct Name(u32);

unsafe impl Sync for Routine {}

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
        for id in cookie.0 + 1..COUNT as u64 {
            let i = id as usize;
            if api.is_some_and(|x| API_IDS[i] != Some(x)) {
                continue;
            }
            return Some((Cookie(id), Interface {
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
pub unsafe fn register_internal(api: ApiId, name: &str, table: &'static [Routine]) {
    register(api, name, Table(NonNull::from(table).cast()))
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
