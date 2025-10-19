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

# Alignment

Some files, such as ELF, require page alignment.
Archivers should account for this and be configurable by the user.
Alignment can easily be checked by testing the low bits of the offset *and* the length.
"""

from pathlib import Path

DEFAULT_ALIGNMENT = 1
MAGIC = b'Lemmings archive'

def determine_alignment(x):
    with x.open('rb') as f:
        header = f.read(4)
        if header == b'\x7fELF':
            return 1 << 12
    return 1

def collect_dir(path, buckets):
    entries = {}
    for x in path.iterdir():
        if x.is_file():
            print(f'f {x}')
            entries[x.name.encode('utf-8')] = x
            buckets.setdefault(determine_alignment(x), set()).add(x)
        elif x.is_dir():
            print(f'd {x}')
            entries[x.name.encode('utf-8')] = collect_dir(x, buckets)
        else:
            print(f'warning: unknown type for {x}, skipping')
    # https://stackoverflow.com/a/47017849
    return dict(sorted(entries.items()))

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
            f.seek(og_pos)
        iter_dir(16, 0)

def create_archive(out, root):
    def align(n):
        assert bin(n).count('1') == 1
        n -= 1
        return lambda x: (x + n) & ~n
    align_dir = align(4)

    # For efficiency, we'll do 2 passes:
    # - one for collecting file names for determining ideal packing (accounting for alignment)
    # - second for copying
    buckets = {}
    fs = collect_dir(Path(root), buckets)
    buckets = {k: sorted(buckets[k]) for k in sorted(buckets, reverse=True)}

    align_files = align(max([1, *(k for k in buckets)]))

    size = 16
    def calc_dir_size(fs):
        return align_dir(4 + (4*2+1)*len(fs) + sum(map(len, fs)))
    def calc_dir_offsets(fs):
        nonlocal size
        offset = size
        assert offset == align_dir(offset)
        size += calc_dir_size(fs)
        child_offsets = {}
        for k, v in fs.items():
            if type(v) is dict:
                child_offsets[k] = calc_dir_offsets(v)
        return offset, child_offsets
    dir_offsets = calc_dir_offsets(fs)
    size = files_start = align_files(size)
    file_offsets = {}
    for align in buckets:
        mask = align - 1
        for x in buckets[align]:
            l = x.stat().st_size
            file_offsets[x] = (size, l)
            size += (l + mask) & ~mask
            del x

    with Path(out).open('wb') as f:
        f.write(MAGIC)
        def u32(x):
            f.write(x.to_bytes(4, byteorder='little'))
        def write_dir(fs, dir_offsets):
            u32(len(fs))
            for k, v in fs.items():
                if type(v) is dict:
                    o, _ = dir_offsets[k]
                    l = calc_dir_size(v)
                else:
                    o, l = file_offsets[v]
                u32(o), u32(l)
            for k, v in fs.items():
                l = len(k) - 1
                if type(v) is dict:
                    l |= 1 << 7
                f.write(bytes([l]) + k)
            f.seek(align_dir(f.tell()))
        def write_dirs(fs, dir_offsets):
            write_dir(fs, dir_offsets)
            for k, v in fs.items():
                if type(v) is dict:
                    write_dirs(v, dir_offsets[k][1])
        write_dirs(fs, dir_offsets[1])

        f.seek(align_files(f.tell()))

        assert f.tell() == files_start, f'{f.tell()} ? {files_start}'
        for x in file_offsets:
            o, l = file_offsets[x]
            print(o, l, x)
            f.seek(o)
            with x.open('rb') as g:
                f.write(g.read(l))

def main(args):
    if args[0] in ('--list', '-l'):
        _, ar = args
        return list_archive_contents(Path(ar))
    return create_archive(*args)


if __name__ == '__main__':
    import sys
    main(sys.argv[1:])
