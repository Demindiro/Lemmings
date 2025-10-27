#!/usr/bin/env python3


def _ensure_minversion():
    from sys import version_info
    if version_info.major < 3 or version_info.minor < 6:
        raise Exception('Python 3.7 or later is required')

_ensure_minversion()
del _ensure_minversion


class Routine:
    __slots__ = 'input', 'output'

    def __init__(self, input: str, output: str):
        assert type(input) is str
        assert type(output) is str
        self.input = input
        self.output = output

    def __repr__(self):
        return f'{self.input} -> {self.output}'

class Type:
    __slots__ = ()

class UnitType(Type):
    __slots__ = 'name',

    def __init__(self, name: str):
        assert type(name) is str
        super().__init__()
        self.name = name

    def resolve_types(self, resolve):
        pass

    def __repr__(self):
        return self.name

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

class SumType(Type):
    __slots__ = 'variants'

    def __init__(self):
        # set is *not* stable! (let alone insertion-order...)
        #self.variants = set()
        self.variants = {}

    def add_variant(self, ty: str):
        assert type(ty) is str
        assert ty not in self.variants
        self.variants[ty] = None

    def resolve_types(self, resolve):
        self.variants = {resolve(x): None for x in self.variants}

    def __repr__(self):
        return ' | '.join(self.variants)


class Door:
    __slots__ = 'api_id', 'name', 'routines', 'types'

    def __init__(self, api_id: int):
        assert type(api_id) is int
        assert 1 <= api_id <= 2**128
        self.api_id = api_id
        self.types = {}
        # NOTE: we rely on insertion order (only guaranteed in Python 3.7+)
        self.routines = {}

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

    def add_routine(self, name: str, routine: Routine):
        assert type(name) is str
        assert type(routine) is Routine
        assert name not in self.routines
        self.routines[name] = routine

    def validate(self):
        self.name

    def __repr__(self):
        return repr({
            'name': self.name,
            'api_id': self.api_id,
            'routines': self.routines,
            'types': self.types,
        })


def parse_idl(text) -> Door:
    from blake3 import blake3

    door = Door(int.from_bytes(blake3(text.encode('utf-8')).digest()[:16], byteorder='little'))

    lines = filter(None, map(str.strip, text.split('\n')))
    strip_split = lambda s, c: map(str.strip, s.split(c))

    def parse_door(line):
        def parse_id_part(s):
            assert len(s) == 8, s
            return int(s, 16)
        name, scope = filter(None, line.split())
        door.set_name(name)
        assert scope == '{'
        for l in lines:
            if l == '}':
                break
            name, l = l.split(' ', 1)
            inp, outp = strip_split(l, '->')
            door.add_routine(name, Routine(inp, outp))

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
            door.add_type(name, Ty(start, until))
        return f

    def parse_pointer(line):
        name, ty = strip_split(line, '=')
        access, ty = strip_split(ty, ' ')
        ty = {
            'constant': ConstantPointerType,
            'shared': SharedPointerType,
            'unique': UniquePointerType,
        }[access](ty)
        door.add_type(name, ty)

    def parse_record(line):
        name, scope = strip_split(line, ' ')
        assert scope == '{'
        ty = RecordType()
        for l in lines:
            if l == '}':
                break
            member_name, member_ty = strip_split(l, '=')
            ty.add_member(member_name, member_ty)
        door.add_type(name, ty)

    def parse_sum(line):
        name, variants = strip_split(line, '=')
        ty = SumType()
        for variant in strip_split(variants, '|'):
            ty.add_variant(variant)
        door.add_type(name, ty)

    def parse_unit(line):
        name = line.strip()
        ty = UnitType(name)
        door.add_type(name, ty)

    vtbl = {
        'door': parse_door,
        'pointer': parse_pointer,
        'record': parse_record,
        'sum': parse_sum,
        'unit': parse_unit,
        'uptr': parse_integer(UPtrType),
        'iptr': parse_integer(SPtrType),
    }
    for s in 'us':
        for x in range(3, 8):
            vtbl[f'{s}{1 << x}'] = parse_integer(globals()[f'{s.upper()}{1 << x}Type'])
        del s, x

    for l in lines:
        keyword, l = l.split(' ', 1)
        vtbl[keyword](l)

    door.validate()
    #door.resolve_types()

    return door


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
