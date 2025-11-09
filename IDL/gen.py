#!/usr/bin/env python3


def _ensure_minversion():
    from sys import version_info
    if version_info.major < 3 or version_info.minor < 6:
        raise Exception('Python 3.7 or later is required')

_ensure_minversion()
del _ensure_minversion


class Type:
    __slots__ = ()

class IntegerType(Type):
    __slots__ = 'start', 'until'

    class _AddressMaxIncl: __repr__ = lambda self: 'address_max(incl)'
    class _AddressMaxExcl: __repr__ = lambda self: 'address_max(excl)'
    ADDRESS_MAX_INCL = _AddressMaxIncl()
    ADDRESS_MAX_EXCL = _AddressMaxExcl()

    def __init__(self, start: int, until: int):
        assert type(start) is int
        assert type(until) is int or until in (IntegerType.ADDRESS_MAX_INCL, IntegerType.ADDRESS_MAX_EXCL)
        super().__init__()
        self.start = start
        self.until = until

    def resolve_types(self, resolve):
        pass

    def __repr__(self):
        return f'{self.__class__.__name__[:-4].lower()} ({self.start:#x} until {hex(self.until) if type(self.until) is int else self.until})'
for s in 'us':
    for x in range(3, 8):
        f = lambda x: 1 << x
        lo, hi = (-f(x - 1), f(x - 1)) if s == 's' else (0, f(x))
        name = f'{s.upper()}{1 << x}Type'
        globals()[name] = type(name, (IntegerType,), {'__slots__': (), 'min': lo, 'max': hi})
        del f, lo, hi, name, x
    del s
class UPtrType(IntegerType): __slots__ = ()
class SPtrType(IntegerType): __slots__ = ()

class NullType(Type):
    __slots__ = 'type',
    def __init__(self, ty: str):
        assert type(ty) is str
        super().__init__()
        self.type = ty
    def __repr__(self):
        return f'null({self.type})'

class PointerType(Type):
    __slots__ = 'deref_type',
    def __init__(self, deref_type: str):
        assert type(deref_type) is str
        super().__init__()
        self.deref_type = deref_type
    def __repr__(self):
        ty = self.__class__.__name__[:-11]
        return f'pointer({ty}) {self.deref_type}'
class ConstantPointerType(PointerType):
    __slots__ = ()
class SharedPointerType(PointerType):
    __slots__ = ()
class UniquePointerType(PointerType):
    __slots__ = ()

class RecordType(Type):
    __slots__ = 'members',

    def __init__(self):
        super().__init__()
        self.members = {}

    def add_member(self, name: str, ty: str):
        assert type(name) is str
        assert type(ty) is str
        assert name not in self.members
        self.members[name] = ty

    def __repr__(self):
        return f'{{ {", ".join(f"{k} = {v}" for k, v in self.members.items())} }}'

class RoutineType(Type):
    __slots__ = 'input', 'output'

    def __init__(self, input: str, output: str):
        assert type(input) is str
        assert type(output) is str
        super().__init__()
        self.input = input
        self.output = output

    def __repr__(self):
        return f'{self.input} -> {self.output}'

class SumType(Type):
    __slots__ = 'variants'

    def __init__(self):
        # set is *not* stable! (let alone insertion-order...)
        #self.variants = set()
        self.variants = {}

    def add_variant(self, ty: str):
        assert type(ty) is str
        if ty in self.variants:
            raise Exception(f'{ty} already defined in sum type')
        self.variants[ty] = None

    def resolve_types(self, resolve):
        self.variants = {resolve(x): None for x in self.variants}

    def __repr__(self):
        return ' | '.join(self.variants)


class Door:
    __slots__ = 'api_id', 'name', 'routines', 'types', 'documentation'

    def __repr__(self):
        return repr({
            'name': self.name,
            'api_id': self.api_id,
            'routines': self.routines,
            'types': self.types,
        })


class DoorBuilder:
    __slots__ = 'name', 'routines', 'types', 'documentation'

    def __init__(self):
        self.types = {}
        # NOTE: we rely on insertion order (only guaranteed in Python 3.7+)
        self.routines = {}
        self.documentation = {}

    def set_name(self, name: str):
        try:
            self.name
            raise Exception('attempt to define Door name multiple times')
        except AttributeError:
            pass
        assert type(name) is str
        assert 1 <= len(name) <= 64
        self.name = name

    def add_type(self, name: str, ty: Type):
        assert type(name) is str
        assert isinstance(ty, Type)
        assert name not in self.types
        self.types[name] = ty

    def resolve_types(self):
        for t in self.types.values():
            t.resolve_types(lambda x: self.types[x])

    def add_routine(self, name: str, routine: RoutineType):
        assert type(name) is str
        assert type(routine) is RoutineType
        assert name not in self.routines
        self.routines[name] = routine

    def finish(self) -> Door:
        door = Door()
        door.name = self.name
        door.routines = self.routines
        door.types = self.types
        door.documentation = self.documentation
        door.documentation[door] = door.documentation[None]
        del door.documentation[None]

        ser = Serializer()

        ser.section(0x0, len(self.routines))
        for name, r in self.routines.items():
            ser.pushstr(name)
            ser.pushstr(r.input)
            ser.pushstr(r.output)

        ser.section(0x1, len(self.types))
        def ser_intptr(ty):
            # uptr is variable size, which is annoying
            # we will assume the only sensible type of limit is a lower bound,
            # as we can't predetermine the absolute maximum upper bound.
            # we don't expect to run on any 8-bit architectures. The next step up is 16-bit then.
            # FIXME we fucked up, constants....
            assert ty.until is IntegerType.ADDRESS_MAX_EXCL
            ser.push16(ty.start & 0xffff)
        def ser_int(bits):
            m = (1 << bits) - 1
            def f(ty):
                assert ty.start < ty.until
                ser.pushN(ty.start & m, bits)
                ser.pushN((ty.until - 1) & m, bits)
            return f
        def ser_null(ty):
            ser.pushstr(ty.type)
        def ser_ptr(ty):
            ser.pushstr(ty.deref_type)
        def ser_record(ty):
            for k, v in ty.members.items():
                ser.pushstr(k)
                ser.pushstr(v)
        def ser_sum(ty):
            for v in sorted(ty.variants):
                ser.pushstr(v)
        def ser_routine(ty):
            ser.pushstr(ty.input)
            ser.pushstr(ty.output)
        tbl = {
            UPtrType: (0x00, ser_intptr),
            U8Type:   (0x01, ser_int(8)),
            U16Type:  (0x02, ser_int(16)),
            U32Type:  (0x03, ser_int(32)),
            U64Type:  (0x04, ser_int(64)),
            SPtrType: (0x10, lambda _: "todo" + 0), # TODO how to handle sptr best?
            S8Type:   (0x11, ser_int(8)),
            S16Type:  (0x12, ser_int(16)),
            S32Type:  (0x13, ser_int(32)),
            S64Type:  (0x14, ser_int(64)),
            ConstantPointerType: (0x20, ser_ptr),
            SharedPointerType:   (0x21, ser_ptr),
            UniquePointerType:   (0x22, ser_ptr),
            RecordType: (0x30, ser_record),
            SumType:    (0x31, ser_sum),
            RoutineType: (0x40, ser_routine),
            NullType: (0x50, ser_null),
        }
        for name in sorted(self.types):
            ty = self.types[name]
            ser.pushstr(name)
            i, f = tbl[type(ty)]
            ser.push8(i)
            f(ty)

        door.api_id = ser.finish()
        return door


class Serializer:
    def __init__(self):
        from blake3 import blake3
        self.hasher = blake3()

    def pushN(self, x: int, n: int):
        assert type(x) is int
        assert 0 <= x < 1 << n
        self.hasher.update(x.to_bytes(n // 8, byteorder='little'))

    def push8(self, x: int):
        self.pushN(x, 8)

    def push16(self, x: int):
        self.pushN(x, 16)

    def push32(self, x: int):
        self.pushN(x, 32)

    def push64(self, x: int):
        self.pushN(x, 64)

    def pushstr(self, s: str):
        assert type(s) is str
        s = s.encode('utf-8')
        self.push8(len(s))
        self.hasher.update(s)

    def section(self, id: int, num_entries: int):
        self.push32(id)
        self.push32(num_entries)

    def finish(self):
        return int.from_bytes(self.hasher.digest()[:16], byteorder='little')


def parse_idl(text) -> Door:

    door = DoorBuilder()

    lines = filter(None, map(str.strip, text.split('\n')))
    strip_split = lambda s, c: map(str.strip, s.split(c))

    def parse_door(line):
        def parse_id_part(s):
            assert len(s) == 8, s
            return int(s, 16)
        name, scope = filter(None, line.split())
        door.set_name(name)
        assert scope == '{'
        del name, scope
        doc = []
        for l in lines:
            if l.startswith('--'):
                doc.append(l[2:].strip())
                continue
            if l == '}':
                break
            name, r = parse_routine(l)
            door.add_routine(name, r)
            door.documentation[r] = tuple(doc)
            doc.clear()

    def parse_integer(Ty):
        def f(line):
            name, num = strip_split(line, '=')
            try:
                x, kw, y = filter(None, num.split())
                f = lambda s: IntegerType.ADDRESS_MAX_INCL if s == 'address_max' else int(s, 0)
                start, until = f(x), f(y)
                if kw == 'until':
                    pass
                elif kw == 'to':
                    until = IntegerType.ADDRESS_MAX_EXCL if until is IntegerType.ADDRESS_MAX_INCL else until + 1
                else:
                    assert 0, repr(kw)
            except ValueError:
                start = int(num, 0)
                until = start + 1
            return name, Ty(start, until)
        return f

    def parse_null(line):
        name, ty = strip_split(line, '=')
        return name, NullType(ty)

    def parse_pointer(line):
        name, ty = strip_split(line, '=')
        access, ty = strip_split(ty, ' ')
        ty = {
            'constant': ConstantPointerType,
            'shared': SharedPointerType,
            'unique': UniquePointerType,
        }[access](ty)
        return name, ty

    def parse_record(line):
        name, scope = strip_split(line, ' ')
        assert scope == '{'
        ty = RecordType()
        for l in lines:
            if l == '}':
                break
            member_name, member_ty = strip_split(l, '=')
            ty.add_member(member_name, member_ty)
        return name, ty

    def parse_routine(line):
        name, f = strip_split(line, '=')
        arg, ret = strip_split(f, '->')
        return name, RoutineType(arg, ret)

    def parse_sum(line):
        name, variants = strip_split(line, '=')
        ty = SumType()
        for variant in strip_split(variants, '|'):
            ty.add_variant(variant)
        return name, ty

    vtbl = {
        'door': parse_door,
        'null': parse_null,
        'pointer': parse_pointer,
        'record': parse_record,
        'routine': parse_routine,
        'sum': parse_sum,
        'uptr': parse_integer(UPtrType),
        'iptr': parse_integer(SPtrType),
    }
    for s in 'us':
        for x in range(3, 8):
            vtbl[f'{s}{1 << x}'] = parse_integer(globals()[f'{s.upper()}{1 << x}Type'])
        del s, x

    comment_buffer = []
    for l in lines:
        if l.startswith('--'):
            comment_buffer.append(l[2:].strip())
            continue
        keyword, l = l.split(' ', 1)
        if keyword == 'door':
            parse_door(l)
            doc_ty = None
        else:
            name, ty = vtbl[keyword](l)
            door.add_type(name, ty)
            doc_ty = ty
        door.documentation[doc_ty] = tuple(comment_buffer)
        comment_buffer.clear()

    return door.finish()


def main(idl_path):
    with open(idl_path, 'r') as f:
        text = f.read()
    idl = parse_idl(text)
    print('Doors:')
    for name in sorted(idl.doors):
        door = idl.doors[name]
        print(' ', name)
        for name, routine in door.routines.items():
            print(' ', ' ', name, '=', routine.input, '->', routine.output)
        del name, door
    print('Types:')
    for name in sorted(idl.types):
        ty = idl.types[name]
        print(' ', name, '=', ty)
        del name


if __name__ == '__main__':
    import sys
    main(*sys.argv[1:])
