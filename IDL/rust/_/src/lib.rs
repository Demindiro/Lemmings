#![no_std]
#![forbid(improper_ctypes_definitions)]

macro_rules! tuple {
    ($T:ident $x:ident $X:ident $($Ts:ident $xs:ident $Xs:ident)*) => {
        tuple!(@ $T $x $X $($xs $Xs)*);
        tuple!($($Ts $xs $Xs)*);
    };
    (@ $T:ident $($x:ident $X:ident)*) => {
        #[repr(C)]
        pub struct $T<$($X,)*>($($X,)*);

        impl<$($X,)*> From<$T<$($X,)*>> for ($($X,)*) {
            fn from($T($($x,)*): $T<$($X,)*>) -> Self {
                ($($x,)*)
            }
        }
        impl<$($X,)*> From<($($X,)*)> for $T<$($X,)*> {
            fn from(($($x,)*): ($($X,)*)) -> Self {
                Self($($x,)*)
            }
        }
    };
    () => {};
}

tuple! {
    Tuple6 f F
    Tuple5 e E
    Tuple4 d D
    Tuple3 c C
    Tuple2 b B
    Tuple1 a A
}
