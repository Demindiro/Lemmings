#!/usr/bin/env python3

with open('spleen-6x12.bin', 'rb') as f:
    n = int.from_bytes(f.read(4), 'little')
    w = int.from_bytes(f.read(2), 'little')
    h = int.from_bytes(f.read(2), 'little')
    print('range count:', n)
    print('dimensions: ', w, 'x', h, sep='')
    print('ranges:')
    total = 0
    for _ in range(n):
        start = int.from_bytes(f.read(4), 'little')
        end   = int.from_bytes(f.read(4), 'little')
        print(' ', start, 'until', end)
        total += end - start
    print('total:', total)
    for _ in range(total):
        print()
        for _ in range(h):
            b = int.from_bytes(f.read(1), 'little')
            print(''.join(' #'[(b >> x) & 1] * 2 for x in range(w))[::-1])
