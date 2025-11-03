#!/usr/bin/env python3
"""
# Format

data.bin begins with the magic value 'Lemmings archive'.
It is immediately followed by the root directory.

## Directory format

A directory starts with a u32 length, then a entries in the format:
- u32 data offset
- u32 data length
Immediately followed by name strings:
- u8 name length (0:6) and type (7:7)
- N name bytes
If the type is 0, it is a regular file.
If the type is 1, it is a directory.
Empty names are not valid. To statically rule out empty names, the length is in the range [1;128].
"""

from pathlib import Path

MAGIC = b'Lemmings archive'

def list_archive_contents(path):
    with path.open('rb') as f:
        magic = f.read(16)
        assert magic == MAGIC, f'invalid magic {magic}'
        def u8():
            return f.read(1)[0]
        def u32(): 
            b = f.read(4)
            assert len(b) == 4
            return int.from_bytes(b, byteorder='little')
        def dump_file(offset, length, depth):
            og_pos = f.tell()
            f.seek(offset)
            STRIDE = 32
            for x in range(0, length, STRIDE):
                print('  ' * depth, end='')
                x = f.read(min(STRIDE, length - x))
                for i, b in enumerate(x):
                    print('', f'{b:02x}', sep='' if i % 4 else ' ', end='')
                print()
            f.seek(og_pos)
        def iter_dir(offset, depth):
            og_pos = f.tell()
            f.seek(offset)
            n = u32()
            entries = [(u32(), u32()) for _ in range(n)]
            for data_offset, data_len in entries:
                namelen_typ = u8()
                namelen, typ = (namelen_typ & 0x7f) + 1, bool(namelen_typ & 0x80)
                name = f.read(namelen)
                assert namelen == len(name)
                name = name.decode('utf-8')
                print('  ' * depth, end='')
                print('fd'[typ], f'0x{data_offset:<8x} 0x{data_offset + data_len:<8x}', name)
                if typ:
                    iter_dir(data_offset, depth + 1)
                else:
                    dump_file(data_offset, data_len, depth + 1)
            f.seek(og_pos)
        iter_dir(16, 0)

def create_archive(out, root):
    log_depth = 0
    def log(s):
        nonlocal log_depth
        print('  ' * log_depth, s, sep='')

    with Path(out).open('wb') as f:
        f.write(MAGIC)

        def u32(x):
            return x.to_bytes(4, byteorder='little')

        def collect_file(path: Path) -> (int, int):
            """
            :returns: offset, length
            """
            offt = f.tell()
            with path.open('rb') as g:
                data = g.read()
                f.write(data)
            return offt, len(data)

        def collect_dir(path: Path) -> (int, int):
            """
            :returns: offset, length
            """
            nonlocal log_depth
            import os
            paths = []
            for x in path.iterdir():
                assert 1 <= len(x.name.encode('utf-8')) <= 128
                if not (x.is_file() or x.is_dir()):
                    log(f'warning: unknown type for {x}, skipping')
                    continue
                paths.append(x)
            paths.sort()
            length = 4 + (4 * 2 * len(paths)) + sum(1 + len(x.name) for x in paths)
            f_cur = f.tell()
            f_end = f_cur + length
            f.seek(f_end, os.SEEK_SET)
            table = []
            for x in paths:
                if x.is_file():
                    log(f'f {x}')
                    table.append(collect_file(x))
                elif x.is_dir():
                    log(f'd {x}')
                    log_depth += 1
                    table.append(collect_dir(x))
                    log_depth -= 1
                else:
                    assert 0, f'unsupported type for {x}'
            f_reset = f.tell()
            f.seek(f_cur, os.SEEK_SET)
            f.write(u32(len(table)))
            f.write(b''.join(u32(x) + u32(y) for x, y in table))
            def name(x):
                n = x.name.encode('utf-8')
                return bytes([(len(n) - 1) | (x.is_dir() << 7)]) + n
            f.write(b''.join(name(x) for x in paths))
            assert f.tell() == f_end, f'{f.tell()} ? {f_end}'
            f.seek(f_reset, os.SEEK_SET)
            return f_cur, length

        collect_dir(Path(root))


def main(args):
    if args[0] in ('--list', '-l'):
        _, ar = args
        return list_archive_contents(Path(ar))
    return create_archive(*args)


if __name__ == '__main__':
    import sys
    main(sys.argv[1:])
