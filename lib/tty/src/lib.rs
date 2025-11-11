#![no_std]
#![no_main]

use core::{fmt, ops, ptr::NonNull};

const CHAR_DATA: (u32, u16, u16, &[u8]) = {
    let &[a, b, c, d, e, f, g, h, ref data @ ..] = include_bytes!("../spleen-6x12.bin");
    let n = u32::from_le_bytes([a, b, c, d]);
    let w = u16::from_le_bytes([e, f]);
    let h = u16::from_le_bytes([g, h]);
    (n, w, h, data)
};
const CHAR_RANGE_COUNT: usize = CHAR_DATA.0 as usize;
const CHAR_WIDTH: u16 = CHAR_DATA.1;
const CHAR_HEIGHT: u16 = CHAR_DATA.2;
const CHAR_DIM: Vec2<u16> = Vec2 {
    x: CHAR_WIDTH,
    y: CHAR_HEIGHT,
};

const CHAR_RANGES: ([u32; CHAR_RANGE_COUNT], [u32; CHAR_RANGE_COUNT], &[u8]) = {
    let mut min @ mut max = [0; CHAR_RANGE_COUNT];
    let mut data = CHAR_DATA.3;
    let mut i = 0;
    while i < min.len() {
        let [a, b, c, d, e, f, g, h, data2 @ ..] = data else {
            panic!("bad data")
        };
        min[i] = u32::from_le_bytes([*a, *b, *c, *d]);
        max[i] = u32::from_le_bytes([*e, *f, *g, *h]);
        data = data2;
        i += 1;
    }
    (min, max, data)
};
const CHAR_RANGE_MIN: [u32; CHAR_RANGE_COUNT] = CHAR_RANGES.0;
const CHAR_RANGE_MAX: [u32; CHAR_RANGE_COUNT] = CHAR_RANGES.1;
const CHAR_COUNT: usize = {
    let mut sum = 0;
    let mut i = 0;
    while i < CHAR_RANGE_COUNT {
        sum += CHAR_RANGES.1[i] - CHAR_RANGES.0[i];
        i += 1;
    }
    sum as usize
};

struct IntT<const N: u16>;

trait Int {
    type T;
}

impl Int for IntT<5> {
    type T = u8;
}
impl Int for IntT<6> {
    type T = u8;
}
impl Int for IntT<7> {
    type T = u8;
}
impl Int for IntT<8> {
    type T = u8;
}
impl Int for IntT<9> {
    type T = u16;
}

type CharBits = [<IntT<{ CHAR_DIM.x }> as Int>::T; CHAR_DIM.y as usize];

const CHARS: [CharBits; CHAR_COUNT] = {
    let mut arr = [[0; CHAR_DIM.y as usize]; CHAR_COUNT];
    let mut i = 0;
    let mut data: &[u8] = CHAR_RANGES.2;
    while i < CHAR_COUNT {
        let mut k = 0;
        while k < CHAR_DIM.y as usize {
            let row;
            match core::mem::size_of_val(&arr[i][k]) {
                1 => {
                    let [a, data2 @ ..] = data else {
                        panic!("bad data")
                    };
                    row = u8::from_le_bytes([*a]) as _;
                    data = data2;
                }
                2 => {
                    let [a, b, data2 @ ..] = data else {
                        panic!("bad data")
                    };
                    row = u16::from_le_bytes([*a, *b]) as _;
                    data = data2;
                }
                _ => unreachable!(),
            }
            arr[i][k] = row;
            k += 1;
        }
        i += 1;
    }
    arr
};

const REPLACEMENT_CHAR: &CharBits = {
    // TODO get font with proper replacement character
    //char_bits(char::REPLACEMENT_CHARACTER).expect("replacement character (\u{fffd}) not defined")
    char_bits('?').expect("replacement character (\u{fffd}) not defined")
};

const fn char_bits(chr: char) -> Option<&'static CharBits> {
    let chr = chr as u32;
    let mut i = 0;
    let mut offt = 0;
    while i < CHAR_RANGE_MIN.len() {
        let (min, max) = (CHAR_RANGE_MIN[i], CHAR_RANGE_MAX[i]);
        if min <= chr && chr < max {
            offt += chr - min;
            return Some(&CHARS[offt as usize]);
        }
        offt += max - min;
        i += 1;
    }
    None
}

pub struct Tty<'a, const BBP: usize> {
    base: NonNull<u8>,
    dim: Vec2<u16>,
    stride: u16,
    cursor: Vec2<u16>,
    scroll: u16,
    chars: &'a mut [char],
}

#[derive(Clone, Copy)]
struct Vec2<T> {
    x: T,
    y: T,
}

impl<'a, const BBP: usize> Tty<'a, BBP> {
    pub fn new(
        base: NonNull<u8>,
        width: u16,
        height: u16,
        stride: u16,
        chars: &'a mut [char],
    ) -> Self {
        let dim = Vec2 {
            x: width,
            y: height,
        };
        let n = (dim / CHAR_DIM).map(usize::from).area();
        assert!(chars.len() >= n, "{} ? {}", chars.len(), n);
        chars.fill(' ');
        Self {
            base,
            dim,
            stride,
            cursor: Vec2 { x: 0, y: 0 },
            scroll: 0,
            chars,
        }
    }

    pub fn flush(&mut self) {
        let dim = self.char_dim();
        for y in 0..dim.y {
            let xl = self.row_mut(y).len();
            for x in 0..xl {
                let c = self.row_mut(y)[x];
                self.render_one(x as u16, y, c);
            }
        }
    }

    fn char_dim(&self) -> Vec2<u16> {
        self.dim / CHAR_DIM
    }

    fn row_mut(&mut self, y: u16) -> &mut [char] {
        let dim = self.char_dim().map(usize::from);
        let y = usize::from(y.wrapping_add(self.scroll)) % dim.y;
        let start = y * dim.x;
        let end = start + dim.x;
        &mut self.chars[start..end]
    }

    fn scroll(&mut self) {
        self.scroll = self.scroll.wrapping_add(1);
        if self.scroll >= self.char_dim().y {
            self.scroll = 0
        }
    }

    fn put(&mut self, c: char) {
        let dim = self.char_dim();
        let mut cursor = self.cursor;
        match c {
            '\n' => {
                cursor.x = 0;
                cursor.y += 1;
            }
            c => {
                self.row_mut(cursor.y)[usize::from(cursor.x)] = c;
                cursor.x += 1;
            }
        }
        if cursor.x >= dim.x {
            cursor.x = 0;
            cursor.y += 1;
        }
        while cursor.y >= dim.y {
            cursor.y -= 1;
            self.scroll();
            self.row_mut(dim.y - 1).iter_mut().for_each(|c| *c = ' ');
        }
        self.cursor = cursor;
    }

    fn render_one(&mut self, x: u16, y: u16, c: char) {
        let bits = char_bits(c).unwrap_or(REPLACEMENT_CHAR);
        let offt = Vec2 { x, y } * CHAR_DIM;
        for (y, &row) in bits.iter().enumerate() {
            for x in 0..CHAR_DIM.x {
                let bit = row >> (CHAR_DIM.x - 1 - x) & 1 != 0;
                let pos = offt + Vec2 { x, y: y as u16 };
                self.put_pixel(pos, bit);
            }
        }
    }

    fn put_pixel(&mut self, pos: Vec2<u16>, on: bool) {
        const { assert!(BBP == 32) };
        let p = if on { u32::MAX } else { 0 };
        let Vec2 { x, y } = pos.map(usize::from);
        unsafe {
            self.base
                .cast::<u32>()
                .byte_add(y * usize::from(self.stride))
                .add(x)
                .write(p);
        }
    }
}

impl<T> Vec2<T> {
    fn map<U, F>(self, f: F) -> Vec2<U>
    where
        F: Fn(T) -> U,
    {
        Vec2 {
            x: f(self.x),
            y: f(self.y),
        }
    }
}

impl<T> Vec2<T>
where
    T: ops::Mul<T, Output = T>,
{
    fn area(self) -> T {
        self.x * self.y
    }
}

impl<T, U> ops::Add<Vec2<U>> for Vec2<T>
where
    T: ops::Add<U>,
{
    type Output = Vec2<T::Output>;

    fn add(self, rhs: Vec2<U>) -> Self::Output {
        Vec2 {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl<T, U> ops::Mul<Vec2<U>> for Vec2<T>
where
    T: ops::Mul<U>,
{
    type Output = Vec2<T::Output>;

    fn mul(self, rhs: Vec2<U>) -> Self::Output {
        Vec2 {
            x: self.x * rhs.x,
            y: self.y * rhs.y,
        }
    }
}

impl<T, U> ops::Div<Vec2<U>> for Vec2<T>
where
    T: ops::Div<U>,
{
    type Output = Vec2<T::Output>;

    fn div(self, rhs: Vec2<U>) -> Self::Output {
        Vec2 {
            x: self.x / rhs.x,
            y: self.y / rhs.y,
        }
    }
}

impl<const BBP: usize> fmt::Write for Tty<'_, BBP> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        s.chars().for_each(|c| self.put(c));
        Ok(())
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        self.put(c);
        Ok(())
    }
}
