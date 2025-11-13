RESERVED_KEYWORDS = frozenset(('type', 'let'))

class Sysv64Type:
    __slots__ = 'memory_size', 'memory_alignment'

    def __init__(self, memory_size: int, memory_alignment: int):
        assert type(memory_size) is int
        assert type(memory_alignment) is int
        self.memory_size = memory_size
        self.memory_alignment = memory_alignment

    def round_memory_size(self):
        """
        Round `memory_size` *up* to `memory_alignment`.
        """
        assert bin(self.memory_alignment).count('1') == 1, "invalid memory alignment"
        m = self.memory_alignment - 1
        self.memory_size = (self.memory_size + m) & ~m


def fix_builtin_ident_collisions(idl):
    """
    Rename names such as `u8` or `type`
    """
    # TODO


def types_to_sysv64(idl):
    """
    Generates SysV64 compatible structures for all types
    """
    import gen

    def from_integer(signed, bits):
        def f(ty):
            start = ty.start
            until = (1<<64) if ty.until is gen.IntegerType.ADDRESS_MAX_EXCL else ty.until
            x = None
            if start < 0:
                for x in range(4):
                    bits = 1 << ((8 << x) - 1)
                    if -bits <= start and until <= bits:
                        break
            else:
                for x in range(4):
                    bits = 1 << (8 << x)
                    if until <= bits:
                        break
            if x is None:
                assert 0, "todo: integers larger than u64"
            return Sysv64Type(1 << x, 1 << x)
        return f

    def from_null(ty):
        return resolve(ty.type)

    def from_pointer(ty):
        return Sysv64Type(8, 8)

    def from_record(ty):
        sysv = Sysv64Type(0, 0)
        for m_name, m_ty in ty.members.items():
            m_sysv = resolve(m_ty)
            sysv.memory_alignment = max(sysv.memory_alignment, m_sysv.memory_alignment)
            # make sure succeeding types are aligned
            sysv.round_memory_size()
            sysv.memory_size += m_sysv.memory_size
        return sysv

    def from_routine(ty):
        return Sysv64Type(8, 8)

    def from_sum(ty):
        sysv = Sysv64Type(0, 0)
        for v_ty in ty.variants:
            v_sysv = resolve(v_ty)
            sysv.memory_size = max(sysv.memory_size, v_sysv.memory_size)
            sysv.memory_alignment = max(sysv.memory_alignment, v_sysv.memory_alignment)
        return sysv

    vtbl = {
        gen.IntegerType: from_integer,
        gen.NullType: from_null,
        gen.RecordType: from_record,
        gen.SumType: from_sum,
        gen.UPtrType: from_integer(False, None),
        gen.SPtrType: from_integer(True, None),
        gen.RoutineType: from_routine,
    }
    for x in ('Constant', 'Shared', 'Unique'):
        vtbl[gen.__dict__[f'{x}PointerType']] = from_pointer
    for s in 'US':
        for x in range(3, 8):
            vtbl[gen.__dict__[f'{s}{1 << x}Type']] = from_integer(s == 'S', 1 << x)

    resolving = set()
    resolved = {}
    def resolve(name):
        if name in resolved:
            return resolved[name]
        if name in resolving:
            raise Exception('recursive type')
        ty = idl.types[name]
        resolving.add(name)
        sysv = resolved[name] = vtbl[type(ty)](ty)
        resolving.remove(name)
        return sysv

    return { name: resolve(name) for name in idl.types }

def emit_ffi(outf, idl, sysv):
    import gen

    outf_depth = 0
    out = lambda s: outf('    ' * outf_depth + s) if s else outf('')

    class Scope:
        def __init__(self, s, *, suffix = ''):
            self.s = s
            self.suffix = suffix
        def __enter__(self):
            nonlocal outf_depth
            out(self.s + ' {' if self.s else '{')
            outf_depth += 1
        def __exit__(self, *_):
            nonlocal outf_depth
            outf_depth -= 1
            out(f'}}{self.suffix}')
    class Impl(Scope):
        def __init__(self, name, *, unsafe = False):
            super().__init__(f'{"unsafe " if unsafe else ""}impl {name}')
    class ImplFor(Impl):
        def __init__(self, trait, name, *, unsafe = False):
            super().__init__(f'{trait} for {name}', unsafe = unsafe)
    class Fn(Scope):
        def __init__(self, name, args, ret, *, vis = 'pub(crate) ', macro_public = False, dead_code = False):
            ret = ret and f' -> {ret}'
            if macro_public:
                public = True
                out('#[doc(hidden)]')
            if dead_code:
                out('#[allow(dead_code)]')
            out('#[inline]')
            super().__init__(f'{vis}fn {name}({args}){ret}')

    def newtype(name, wrap):
        out('#[derive(Clone, Copy)]')
        out('#[repr(transparent)]')
        out(f'pub struct {name}(pub {wrap});')

    def emit_integer(signed, bits):
        x = f'{"ui"[signed]}{bits or "size"}'
        until_limit = 1 << bits if bits else gen.IntegerType.ADDRESS_MAX_EXCL
        def f(name, ty, sysv_ty):
            newtype(name, x)
            is_unit = ty.start + 1 == ty.until
            full_range = ty.start == 0 and ty.until == until_limit
            if is_unit:
                with Scope(f'impl Default for {name}'):
                    with Scope('fn default() -> Self'):
                        out(f'Self({ty.start})')
            with Impl(name):
                with Fn('is_valid', f'self', 'bool', dead_code = True):
                    if full_range:
                        out('true')
                    elif ty.start == 0:
                        if ty.start == ty.until - 1:
                            out(f'{ty.start:#x} == self.0')
                        else:
                            out(f'self.0 <= {ty.until - 1:#x}')
                    else:
                        if ty.until == until_limit:
                            out(f'{ty.start:#x} <= self.0')
                        elif ty.start == ty.until - 1:
                            out(f'{ty.start:#x} == self.0')
                        else:
                            out(f'{ty.start:#x} <= self.0 && self.0 <= {ty.until - 1:#x}')
            with ImplFor(f'From<{name}>', x):
                with Fn('from', f'x: {name}', 'Self', vis = ''):
                    out('x.0')
        return f

    def emit_null(name, ty, sysv_ty):
        x = idl.types[name].type
        newtype(name, x)
        with Impl(name):
            with Fn('is_valid', f'self', 'bool', dead_code = True):
                out('self.0.0.is_none()')
        with ImplFor('Default', name):
            with Fn('default', '', 'Self', vis = ''):
                out(f'Self({x}(None))')

    def emit_pointer(name, ty, sysv_ty):
        newtype(name, f'Option<NonNull<{ty.deref_type}>>')
        with Impl(name):
            with Fn('is_valid', f'self', 'bool', dead_code = True):
                out('self.0.is_some()')

    def emit_record(name, ty, sysv_ty):
        tr = lambda x: f'r#{x}' if x in RESERVED_KEYWORDS else x
        def members():
            yield from ((tr(k), v) for k, v in ty.members.items())
        out(f'#[derive(Clone, Copy)]')
        out(f'#[repr(C)]')
        with Scope(f'pub struct {name}'):
            for m_name, m_ty in ty.members.items():
                out(f'pub {tr(m_name)}: {m_ty},')
        with Impl(name):
            with Fn('is_valid', '&self', 'bool', dead_code = True):
                out(' && '.join(f'self.{m_name}.is_valid()' for m_name, m_ty in members()))

    def emit_routine(name, ty, sysv_ty):
        newtype(name, f'Option<unsafe extern "sysv64" fn()>')
        with Impl(name):
            with Fn('is_valid', f'self', 'bool', dead_code = True):
                out('self.0.is_some()')

    def emit_sum(name, ty, sysv_ty):
        out(f'#[derive(Clone, Copy)]')
        out(f'#[repr(C)]')
        with Scope(f'pub union {name}'):
            for v in ty.variants:
                out(f'pub {v}: {v},')
        with Impl(name):
            with Fn('is_valid', f'&self', 'bool', dead_code = True):
                with Scope('unsafe'):
                    out(' || '.join(f'self.{v}.is_valid()' for v in ty.variants))

    vtbl = {
        gen.NullType: emit_null,
        gen.RecordType: emit_record,
        gen.SumType: emit_sum,
        gen.UPtrType: emit_integer(False, None),
        gen.SPtrType: emit_integer(True, None),
        gen.RoutineType: emit_routine,
    }
    for x in ('Constant', 'Shared', 'Unique'):
        vtbl[gen.__dict__[f'{x}PointerType']] = emit_pointer
    for s in 'US':
        for x in range(3, 8):
            vtbl[gen.__dict__[f'{s}{1 << x}Type']] = emit_integer(s == 'S', 1 << x)

    out('#[doc(hidden)]')
    with Scope('pub mod ffi'):
        out('#![allow(non_snake_case)]')
        out('')
        if any(isinstance(t, gen.PointerType) for t in idl.types.values()):
            out('use core::ptr::NonNull;')
            out('')
        for name, ty in idl.types.items():
            sysv_ty = sysv[name]
            if sysv_ty.memory_size == 0:
                # do emit for pointers and stuff
                out(f'pub struct {name};')
                continue
            vtbl[type(ty)](name, ty, sysv_ty)
            out('')

def emit(outf, idl, sysv):
    import gen

    emit_ffi(outf, idl, sysv)

    outf_depth = 0
    out = lambda s: outf('    ' * outf_depth + s) if s else outf('')

    tr = lambda x: f'r#{x}' if x in RESERVED_KEYWORDS else x

    if any(isinstance(t, gen.PointerType) for t in idl.types.values()):
        out('use core::ptr::NonNull;')
        out('')

    class Scope:
        def __init__(self, s, *, suffix = ''):
            self.s = s
            self.suffix = suffix
        def __enter__(self):
            nonlocal outf_depth
            out(self.s + ' {' if self.s else '{')
            outf_depth += 1
        def __exit__(self, *_):
            nonlocal outf_depth
            outf_depth -= 1
            out(f'}}{self.suffix}')
    class Impl(Scope):
        def __init__(self, name, *, unsafe = False):
            super().__init__(f'{"unsafe " if unsafe else ""}impl {name}')
    class ImplFor(Impl):
        def __init__(self, trait, name, *, unsafe = False):
            super().__init__(f'{trait} for {name}', unsafe = unsafe)
    class Fn(Scope):
        def __init__(self, name, args, ret, *, public = False, macro_public = False, dead_code = False):
            ret = ret and f' -> {ret}'
            if macro_public:
                public = True
                out('#[doc(hidden)]')
            if dead_code:
                out('#[allow(dead_code)]')
            out('#[inline]')
            public = 'pub ' if public else ''
            super().__init__(f'{public}fn {name}({args}){ret}')

    def documentation(ty):
        return idl.documentation.get(ty, tuple())
    def emit_documentation(ty):
        for l in documentation(ty):
            out(f'/// {l}')

    def is_void(name):
        sysv_ty = sysv[name]
        # be defensive about alignment, just in case
        return name == 'Void' and sysv_ty.memory_size == 0 and sysv_ty.memory_alignment <= 1
    def is_unit(name):
        ty = idl.types[name]
        if type(ty) is gen.NullType:
            return True
        if isinstance(ty, gen.IntegerType):
            if ty.start + 1 == ty.until:
                return True
        return False

    def ffi_name(name, macro) -> str:
        return f'{"$crate::" if macro else ""}ffi::{name}'
    def unpack_record_members(ty, *, prefix = '') -> str:
        return ", ".join(f"{tr(m_name)}" for m_name in ty.members)
    def unpack_record_pattern(name, ty) -> str:
        return f'{name} {{ {unpack_record_members(ty)} }}'
    def should_unpack(name) -> bool:
        ty = idl.types[name]
        if type(ty) is gen.SumType:
            return should_unpack(next(iter(ty.variants)))
        return type(ty) is gen.RecordType and not is_void(name)
    def sysv_splat_params(name, *, macro = False) -> str:
        if should_unpack(name):
            ty = idl.types[name]
            return ', '.join(f'{tr(m_name)}: {ffi_name(m_ty, macro)}' for m_name, m_ty in ty.members.items())
        return f'x: {ffi_name(name, macro)}' if sysv[name].memory_size > 0 else ''
    def sysv_splat_ret(name, *, macro = False) -> str:
        return '' if is_void(name) else f'-> {ffi_name(name, macro)}'
    def sysv_splat_members(name, *, macro = False) -> str:
        if is_void(name):
            return ''
        ty = idl.types[name]
        if should_unpack(name):
            return unpack_record_members(ty)
        return 'x'
    def sysv_splat_pattern(name, *, macro = False) -> str:
        if is_void(name):
            return ''
        ty = idl.types[name]
        if should_unpack(name):
            return unpack_record_pattern(ffi_name(name, macro), ty)
        return 'x'

    emit_documentation(idl)
    out(f'#[derive(Debug)]')
    out(f'#[repr(C)]')
    with Scope(f'pub struct {idl.name.replace("_", "")}'):
        for name, routine in idl.routines.items():
            args = sysv_splat_params(routine.input)
            ret = sysv_splat_ret(routine.output)
            emit_documentation(routine)
            out(f'pub {name}: unsafe extern "sysv64" fn({args}){ret},')
            del name, routine, args, ret
    out('')

    def emit_integer(signed, bits):
        x = f'{"ui"[signed]}{bits or "size"}'
        until_limit = 1 << bits if bits else gen.IntegerType.ADDRESS_MAX_EXCL
        def f(name, ty, sysv):
            # Don't bother with unit integers
            # They are only relevant for sum types
            if is_unit(name):
                # ... but do emit a unit type for convenience with sum types
                out(f'pub struct {name};')
                return
            full_range = ty.start == 0 and ty.until == until_limit
            emit_documentation(ty)
            out(f'#[derive(Clone, Debug)]')
            out(f'pub struct {name}({x});')
            with Impl(name):
                with Fn('from_ffi', f'x: ffi::{name}', 'Self', macro_public = True):
                    out(f'Self(x.0)')
                with Fn('to_ffi', 'self', f'ffi::{name}', macro_public = True):
                    out(f'ffi::{name}(self.0)')
            with ImplFor(f'From<{name}>', x):
                with Fn('from', f'x: {name}', 'Self'):
                    out('x.0')
            if full_range:
                with ImplFor(f'From<{x}>', name):
                    with Fn('from', f'x: {x}', 'Self'):
                        out('Self(x)')
            else:
                with ImplFor(f'TryFrom<{x}>', name):
                    out('type Error = ();')
                    with Fn('try_from', f'x: {x}', 'Result<Self, Self::Error>'):
                        out(f'ffi::{name}(x).is_valid().then_some(Self(x)).ok_or(())')
            if type(ty.until) is int and ty.start == ty.until - 1:
                with ImplFor('Default', name):
                    with Fn('default', '', 'Self'):
                        out(f'Self({ty.start:#x})')
            '''
            with ImplFor(f'TryFrom<{x}>', name):
                out('type Error = ();')
                with Fn('try_from', f'x: {x}', 'Result<Self, Self::Error>'):
                    out('')
            '''
        return f

    def emit_null(name, ty, sysv):
        pass

    def emit_pointer(name, ty, sysv):
        x = f'NonNull<ffi::{ty.deref_type}>'
        emit_documentation(ty)
        out(f'#[derive(Clone, Debug)]')
        out(f'pub struct {name}(pub {x});')
        with Impl(name):
            # TODO check for null or nay?
            with Fn('from_ffi', f'x: ffi::{name}', 'Self', macro_public = True):
                out('Self(x.0.expect("pointer is null"))')
            with Fn('to_ffi', 'self', f'ffi::{name}', macro_public = True):
                out(f'ffi::{name}(Some(self.0))')
        with ImplFor(f'From<{x}>', name):
            with Fn('from', f'x: {x}', 'Self'):
                out(f'Self(x)')

    def emit_record(name, ty, sysv_ty):
        if is_void(name):
            return
        emit_documentation(ty)
        out(f'#[derive(Clone, Debug)]')
        if sysv_ty.memory_size == 0:
            out(f'pub struct {name};')
            return
        def members():
            yield from ((tr(k), v) for k, v in ty.members.items())
        with Scope(f'pub struct {name}'):
            for m_name, m_ty in members():
                if is_unit(m_ty):
                    continue
                out(f'pub {m_name}: {m_ty},')
        out('')
        with Impl(name):
            with Fn('from_ffi', f'x: ffi::{name}', 'Self', macro_public = True):
                with Scope('Self'):
                    for m_name, m_ty in members():
                        if is_unit(m_ty):
                            continue
                        out(f'{m_name}: {m_ty}::from_ffi(x.{m_name}),')
                    del m_name, m_ty
            with Fn('to_ffi', 'self', f'ffi::{name}', macro_public = True):
                with Scope(f'ffi::{name}'):
                    for m_name, m_ty in members():
                        expr = f'ffi::{m_ty}::default()' if is_unit(m_ty) else f'self.{m_name}.to_ffi()'
                        out(f'{m_name}: {expr},')
                    del m_name, m_ty, expr

    def emit_routine(name, ty, sysv_ty):
        emit_documentation(ty)
        out(f'#[derive(Clone, Debug)]')
        out(f'pub struct {name}(pub unsafe extern "sysv64" fn());')
        with Impl(name):
            with Fn('from_ffi', f'x: ffi::{name}', 'Self'):
                out('Self(x.0.expect("function pointer is null"))')
            with Fn('to_ffi', 'self', f'ffi::{name}'):
                out(f'ffi::{name}(Some(self.0))')

    def emit_sum(name, ty, sysv_ty):
        emit_documentation(ty)
        out(f'#[derive(Clone, Debug)]')
        with Scope(f'pub enum {name}'):
            for v in ty.variants:
                out(f'{v},' if is_unit(v) else f'{v}({v}),')
        if sysv_ty.memory_size == 0:
            return
        with Impl(name):
            with Fn('from_ffi', f'x: ffi::{name}', 'Self', macro_public = True):
                with Scope('unsafe'):
                    with Scope('match x'):
                        for v in ty.variants:
                            expr = '' if is_unit(v) else f'({v}::from_ffi(x.{v}))'
                            out(f'x if x.{v}.is_valid() => Self::{v}{expr},')
                        del v, expr
                        out('_ => panic!("invalid variant"),')
            with Fn('to_ffi', 'self', f'ffi::{name}', macro_public = True):
                with Scope('match self'):
                    for v in ty.variants:
                        if is_unit(v):
                            out(f'Self::{v} => ffi::{name} {{ {v}: ffi::{v}::default() }},')
                        else:
                            out(f'Self::{v}(x) => ffi::{name} {{ {v}: x.to_ffi() }},')
        for v in ty.variants:
            with ImplFor(f'From<{v}>', name):
                if is_unit(v):
                    with Fn('from', f'_: {v}', 'Self'):
                        out(f'Self::{v}')
                else:
                    with Fn('from', f'x: {v}', 'Self'):
                        out(f'Self::{v}(x)')

    vtbl = {
        gen.IntegerType: emit_integer,
        gen.NullType: emit_null,
        gen.RecordType: emit_record,
        gen.SumType: emit_sum,
        gen.UPtrType: emit_integer(False, None),
        gen.SPtrType: emit_integer(True, None),
        gen.RoutineType: emit_routine,
    }
    for x in ('Constant', 'Shared', 'Unique'):
        vtbl[gen.__dict__[f'{x}PointerType']] = emit_pointer
    for s in 'US':
        for x in range(3, 8):
            vtbl[gen.__dict__[f'{s}{1 << x}Type']] = emit_integer(s == 'S', 1 << x)

    for name, ty in idl.types.items():
        vtbl[type(ty)](name, ty, sysv[name])
        out('')

    with ImplFor('lemmings_idl::Api', idl.name, unsafe = True):
        out(f'const ID: core::num::NonZero<u128> = core::num::NonZero::new({idl.api_id:#x}).unwrap();')

    with Impl(idl.name):
        for name, routine in idl.routines.items():
            emit_documentation(routine)
            x = '' if sysv[routine.input].memory_size == 0 else 'x'
            params = '' if is_void(routine.input) else f', {x or "_"}: {routine.input}'
            ret = '' if is_void(routine.output) else routine.output
            with Fn(name, f'&self{params}', ret, public = True):
                if x:
                    out(f'let {sysv_splat_pattern(routine.input)} = x.to_ffi();')
                args = sysv_splat_members(routine.input)
                if sysv[routine.output].memory_size > 0:
                    out(f'let x = unsafe {{ (self.{name})({args}).into() }};')
                    out(f'{routine.output}::from_ffi(x)')
                else:
                    out(f'unsafe {{ (self.{name})({args}) }};')
                    if not is_void(routine.output):
                        out(f'{routine.output}')
        del name, routine, x

    out('')
    out('#[macro_export]')
    with Scope('macro_rules! imp'):
        with Scope('', suffix = ' =>'):
            out(f'[{idl.name}]')
            for name, routine in idl.routines.items():
                out(f'{name} = $impl_{name}:expr,')
        with Scope('', suffix = ';'):
            with Scope(f'$crate::{idl.name}'):
                for name, routine in idl.routines.items():
                    args = sysv_splat_params(routine.input, macro = True)
                    ret = sysv_splat_ret(routine.output, macro = True)
                    with Scope(f'{name}:', suffix = ','):
                        fn = f'unsafe extern "sysv64" fn ffi_{name}({args})'
                        if not is_void(routine.output):
                            fn = f'{fn}{ret}'
                        with Scope(fn):
                            x = 'x'
                            if is_void(routine.input):
                                x = ''
                            elif sysv[routine.input].memory_size > 0:
                                out(f'let x = {routine.input}::from_ffi({sysv_splat_pattern(routine.input, macro = True)});')
                            else:
                                out(f'let x = {routine.input};')
                            if is_void(routine.output):
                                out(f'$impl_{name}({x})')
                            elif sysv[routine.output].memory_size > 0:
                                out(f'let x: {routine.output} = $impl_{name}({x});')
                                out(f'x.to_ffi().into()')
                            else:
                                out(f'let _: {routine.output} = $impl_{name}({x});')
                        out(f'ffi_{name}')
        del name, routine

def main(idl_path, out_path):
    import gen
    from pathlib import Path
    with open(idl_path, 'r') as f:
        idl = f.read()
    idl = gen.parse_idl(idl)
    print(idl.name, '->', hex(idl.api_id))

    fix_builtin_ident_collisions(idl)
    sysv64 = types_to_sysv64(idl)

    out_path = Path(out_path)
    (out_path / 'src').mkdir(parents=True, exist_ok=True)
    (out_path / 'Cargo.toml').open('w').write(f'''\
[package]
name = "lemmings-idl-{out_path.name}"
version = "0.1.0"
edition = "2024"

[dependencies]
lemmings-idl = {{ path = "../_" }}
''')
    with (out_path / 'src' / 'lib.rs').open('w') as f:
        f.write('#![no_std]\n')
        f.write('#![forbid(improper_ctypes_definitions)]\n')
        f.write('\n')
        f.write('pub use lemmings_idl;\n')
        f.write('\n')
        emit(lambda s: f.write(f'{s}\n'), idl, sysv64)


if __name__ == '__main__':
    import sys
    main(*sys.argv[1:])
