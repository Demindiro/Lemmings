class Sysv64Type:
    """
    There are two data layouts to consider:
    - One where all values are passed in registers
    - One where all values are passed through memory
    For registers we will want to have one value per register.
    For memory we will need to account for alignment.

    We also assume there exists no "sysv64" ABI for targets with distinct
    integer and address register sets.
    """
    __slots__ = 'register_count', 'memory_size', 'memory_alignment'

    def __init__(self, register_count: int, memory_size: int, memory_alignment: int):
        assert type(register_count) is int
        assert type(memory_size) is int
        assert type(memory_alignment) is int
        self.register_count = register_count
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
            return Sysv64Type(1, 1 << x, 1 << x)
        return f

    def from_pointer(ty):
        return Sysv64Type(1, 8, 8)

    def from_record(ty):
        sysv = Sysv64Type(0, 0, 0)
        for m_name, m_ty in ty.members.items():
            m_sysv = resolve(m_ty)
            sysv.register_count += m_sysv.register_count
            sysv.memory_alignment = max(sysv.memory_alignment, m_sysv.memory_alignment)
            # make sure succeeding types are aligned
            sysv.round_memory_size()
            sysv.memory_size += m_sysv.memory_size
        return sysv

    def from_routine(ty):
        return Sysv64Type(1, 8, 8)

    def from_sum(ty):
        sysv = Sysv64Type(0, 0, 0)
        for v_ty in ty.variants:
            v_sysv = resolve(v_ty)
            sysv.register_count = max(sysv.register_count, v_sysv.register_count)
            sysv.memory_size = max(sysv.memory_size, v_sysv.memory_size)
            sysv.memory_alignment = max(sysv.memory_alignment, v_sysv.memory_alignment)
        return sysv

    vtbl = {
        gen.IntegerType: from_integer,
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


def emit(outf, idl, sysv):
    import gen

    VARS = 'abcdefghijklmnopqrstuvwxyz'
    RESERVED_KEYWORDS = frozenset(('type', 'let'))
    USIZE_MASK = (1 << 64) - 1

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
        def __init__(self, name):
            super().__init__(f'impl {name}')
    class ImplFor(Impl):
        def __init__(self, trait, name):
            super().__init__(f'{trait} for {name}')
    class Fn(Scope):
        def __init__(self, name, args, ret, *, public = False, macro_public = False, dead_code = False):
            ret = ret and f' -> {ret}'
            if macro_public:
                public = True
                out('#[doc(hidden)]')
            if dead_code:
                out('#[allow(dead_code)]')
            public = 'pub ' if public else ''
            super().__init__(f'{public}fn {name}({args}){ret}')

    def documentation(ty):
        return idl.documentation.get(ty, tuple())
    def emit_documentation(ty):
        for l in documentation(ty):
            out(f'/// {l}')

    def regn_to_usizes(sysv_ty):
        n = sysv_ty.register_count
        if n == 0:
            return ''
        if n == 1:
            return 'usize'
        return f'({", ".join("usize" for _ in range(n))})'
    def regn_to_usizes_args(sysv_ty):
        n = sysv_ty.register_count
        return ', '.join(f'{x}: usize' for x in VARS[:n])
    def regn_to_usizes_vars(sysv_ty):
        n = sysv_ty.register_count
        v = ', '.join(VARS[:n])
        return v if n < 2 else f'({v})'
    def regn_to_usizes_ret(sysv_ty, *, macro = False):
        n = sysv_ty.register_count
        assert n <= 2, "todo: more than 2 output registers"
        v = ', '.join(['usize'] * n)
        outputs = v if n < 2 else f'lemmings_idl::Tuple{n}<{v}>'
        if outputs and macro and n >= 2:
            outputs = f'$crate::{outputs}'
        return outputs and f' -> {outputs}'

    emit_documentation(idl)
    out(f'#[repr(C)]')
    with Scope(f'pub struct {idl.name.replace("_", "")}'):
        for name, routine in idl.routines.items():
            args = regn_to_usizes_args(sysv.get(routine.input))
            ret = regn_to_usizes_ret(sysv.get(routine.output))
            emit_documentation(routine)
            out(f'pub {name}: unsafe extern "sysv64" fn({args}){ret},')
            del name, routine, args, ret
    out('')

    def emit_integer(signed, bits):
        x = f'{"ui"[signed]}{bits or "size"}'
        def f(name, ty, sysv):
            emit_documentation(ty)
            out(f'#[derive(Clone, Debug)]')
            out(f'pub struct {name}({x});')
            with Impl(name):
                always_true = ty.start == 0 and ty.until in (gen.IntegerType.ADDRESS_MAX_EXCL, 1 << 64)
                with Fn('is_valid', f'{"x_"[always_true]}: usize', 'bool', dead_code = True):
                    if always_true:
                        out('true')
                    elif ty.start == 0:
                        if ty.start == ty.until - 1:
                            # FIXME properly
                            out(f'{ty.start & USIZE_MASK:#x} == x')
                        else:
                            out(f'x <= {ty.until - 1:#x}')
                    else:
                        if ty.until is gen.IntegerType.ADDRESS_MAX_EXCL or ty.until == (1 << 64):
                            out(f'{ty.start:#x} <= x')
                        elif ty.start == ty.until - 1:
                            # FIXME properly
                            out(f'{ty.start & USIZE_MASK:#x} == x')
                        else:
                            out(f'{ty.start:#x} <= x && x <= {ty.until - 1:#x}')
                with Fn('from_ffi', 'x: usize', 'Self', macro_public = True):
                    e = f'{x[-2:].lower()}::try_from(x).unwrap()' if x.startswith('core::num::NonZero') else 'x'
                    out(f'Self({e}.try_into().expect("FFI conversion failed: {name} out of range"))')
                with Fn('to_ffi', 'self', 'usize', macro_public = True):
                    out(f'self.0.try_into().expect("{x} doesn\'t fit in usize")')
            with ImplFor(f'From<{name}>', x):
                with Fn('from', f'x: {name}', 'Self'):
                    out('x.0')
            # FIXME don't hardcode 64
            if ty.start == 0 and ty.until == 1 << (bits or 64):
                with ImplFor(f'From<{x}>', name):
                    with Fn('from', f'x: {x}', 'Self'):
                        out('Self(x)')
            else:
                with ImplFor(f'TryFrom<{x}>', name):
                    out('type Error = ();')
                    with Fn('try_from', f'x: {x}', 'Result<Self, Self::Error>'):
                        # FIXME this usize nonsense is making things way too hard
                        out('Self::is_valid(x as _).then_some(Self(x)).ok_or(())')
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

    def emit_pointer(name, ty, sysv):
        emit_documentation(ty)
        out(f'#[derive(Clone, Debug)]')
        out(f'pub struct {name}(pub core::ptr::NonNull<{ty.deref_type}>);')
        with Impl(name):
            with Fn('is_valid', 'x: usize', 'bool', dead_code = True):
                out('x != 0')
            with Fn('from_ffi', 'x: usize', 'Self', macro_public = True):
                out('Self(core::ptr::NonNull::new(x as *mut _).expect("pointer is null"))')
            with Fn('to_ffi', 'self', 'usize', macro_public = True):
                out('self.0.as_ptr() as usize')

    def emit_record(name, ty, sysv_ty):
        tr = lambda x: f'r#{x}' if x in RESERVED_KEYWORDS else x
        emit_documentation(ty)
        out(f'#[derive(Clone, Debug)]')
        if sysv_ty.register_count == 0:
            out(f'pub struct {name};')
            return
        with Scope(f'pub struct {name}'):
            for m_name, m_ty in ty.members.items():
                out(f'pub {tr(m_name)}: {m_ty},')
        out('')
        vals = regn_to_usizes(sysv_ty)
        args = vals and f'x: {vals}'
        with Impl(name):
            with Fn('is_valid', args, 'bool', dead_code = True):
                i = 0
                for m_name, m_ty in ty.members.items():
                    prefix = '' if i == 0 else '&& '
                    n = sysv[m_ty].register_count
                    values = f'x.{i}' if n == 1 else f'({", ".join(f"x.{k}" for k in range(i, i + n))})'
                    out(f'{prefix}{m_ty}::is_valid({values})')
                    i += n
                    del values, m_name, m_ty, n
                del i
            with Fn('from_ffi', args, 'Self', macro_public = True):
                with Scope('Self'):
                    i = 0
                    for m_name, m_ty in ty.members.items():
                        n = sysv[m_ty].register_count
                        values = f'x.{i}' if n == 1 else f'({", ".join(f"x.{k}" for k in range(i, i + n))})'
                        out(f'{tr(m_name)}: {m_ty}::from_ffi({values}),')
                        i += n
                        del values, m_name, m_ty, n
                    del i
            with Fn('to_ffi', 'self', vals, macro_public = True):
                i = 0
                for m_name, m_ty in ty.members.items():
                    n = sysv[m_ty].register_count
                    y = f'({", ".join(VARS[i:i+n])})' if n > 1 else VARS[i]
                    out(f'let {y} = self.{tr(m_name)}.to_ffi();')
                    i += n
                    del m_name, m_ty, n, y
                out(VARS[0] if i == 1 else f'({", ".join(VARS[:i])})')
                del i

    def emit_routine(name, ty, sysv_ty):
        emit_documentation(ty)
        out(f'#[derive(Clone, Debug)]')
        out(f'pub struct {name}(pub unsafe extern "sysv64" fn());')
        with Impl(name):
            with Fn('is_valid', 'x: usize', 'bool', dead_code = True):
                out('x != 0')
            with Fn('from_ffi', 'x: usize', 'Self'):
                out('assert_ne!(x, 0, "Function pointer is null");')
                out('Self(unsafe { core::mem::transmute(x) })')
            with Fn('to_ffi', 'self', 'usize'):
                out('self.0 as _')

    def emit_sum(name, ty, sysv_ty):
        emit_documentation(ty)
        out(f'#[derive(Clone, Debug)]')
        if len(ty.variants) == 1:
            # FIXME no worky!
            out(f'pub type {name} = {ty.variants[0]}')
            return
        with Scope(f'pub enum {name}'):
            for v in ty.variants:
                out(f'{v}({v}),')
        if sysv_ty.register_count == 0:
            return
        with Impl(name):
            vals = regn_to_usizes(sysv_ty)
            args = f'x: {vals}'
            with Fn('is_valid', args, 'bool', dead_code = True):
                prefix = ''
                for m_ty in ty.variants:
                    out(f'{prefix}{m_ty}::is_valid(x)')
                    prefix = '|| '
            with Fn('from_ffi', args, 'Self', macro_public = True):
                with Scope('match x'):
                    for v in ty.variants:
                        out(f'x if {v}::is_valid(x) => Self::{v}({v}::from_ffi(x)),')
                        del v
                    out('_ => panic!("invalid variant"),')
            with Fn('to_ffi', 'self', vals, macro_public = True):
                with Scope('match self'):
                    # FIXME proper ID assignment!
                    for i, v in enumerate(ty.variants):
                        out(f'Self::{v}(x) => x.to_ffi(),')

    vtbl = {
        gen.IntegerType: emit_integer,
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

    with Impl(idl.name):
        out(f'pub const ID: core::num::NonZero<u128> = core::num::NonZero::new({idl.api_id:#x}).unwrap();')
        out('')
        for name, routine in idl.routines.items():
            emit_documentation(routine)
            with Fn(name, f'&self, x: {routine.input}', routine.output, public = True):
                n = sysv[routine.input].register_count
                args = ", ".join(VARS[:n])
                if n > 0:
                    y = f'({args})' if n > 1 else args
                    out(f'let {y} = x.to_ffi();')
                if sysv[routine.output].register_count > 0:
                    out(f'let x = unsafe {{ (self.{name})({args}).into() }};')
                    out(f'{routine.output}::from_ffi(x)')
                else:
                    out(f'unsafe {{ (self.{name})({args}) }};')
                    out(f'{routine.output}')
                del n, args
        del name, routine

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
                    sysv_in = sysv[routine.input]
                    sysv_out = sysv[routine.output]
                    args = regn_to_usizes_args(sysv_in)
                    ret = regn_to_usizes_ret(sysv_out, macro = True)
                    with Scope(f'{name}:', suffix = ','):
                        with Scope(f'unsafe extern "sysv64" fn ffi({args}){ret}'):
                            if sysv[routine.input].register_count > 0:
                                out(f'let x = {routine.input}::from_ffi({regn_to_usizes_vars(sysv_in)});')
                            else:
                                out(f'let x = {routine.input};')
                            if sysv[routine.output].register_count > 0:
                                out(f'let x: {routine.output} = $impl_{name}(x);')
                                out(f'x.to_ffi().into()')
                            else:
                                out(f'let _: {routine.output} = $impl_{name}(x);')
                        out(f'ffi')
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
