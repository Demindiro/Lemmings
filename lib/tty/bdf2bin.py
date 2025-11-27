#!/usr/bin/env python3

CHAR_DIM = 6, 12

def parse_char(f, w):
    encoding = w = h = bitmap = None
    def cmd_encoding(arg):
        nonlocal encoding
        encoding, = map(int, arg)
    def cmd_bbx(arg):
        nonlocal w, h
        w, h, x, y = map(int, arg)
    def cmd_bitmap(_):
        nonlocal bitmap
        bitmap = []
        for _ in range(h):
            v = int(next(f), 16)
            v >>= (8 - w) % 8
            bitmap.append(v)
    for l in f:
        cmd, *arg = l.split()
        cmd = cmd.strip()
        if cmd == 'ENDCHAR':
            break
        {
            'ENCODING': cmd_encoding,
            'SWIDTH': lambda _: None,
            'DWIDTH': lambda _: None,
            'BBX': cmd_bbx,
            'BITMAP': cmd_bitmap,
        }[cmd](arg)
    return encoding, bitmap

def parse_chars(f, w):
    chars = {}
    for _ in range(int(w)):
        cmd, w = next(f).split(' ', 1)
        enc, bitmap = {
            'STARTCHAR': lambda w: parse_char(f, w)
        }[cmd](w)
        chars[enc] = bitmap
    return chars

def parse_properties(f):
    for l in f:
        if l.strip() == 'ENDPROPERTIES':
            break

def parse_font(f):
    size = w = h = chars = None
    def cmd_size(w):
        nonlocal size
        _, _, _ = size = w
    def cmd_fontboundingbox(arg):
        nonlocal w, h
        w, h, _, _ = map(int, arg)
    def cmd_chars(arg):
        nonlocal chars
        chars = parse_chars(f, *arg)
    for l in f:
        cmd, *arg = l.split()
        if cmd == 'ENDFONT':
            break
        {
            'COMMENT': lambda _: None,
            'FONT': lambda _: None,
            'SIZE': cmd_size,
            'FONTBOUNDINGBOX': cmd_fontboundingbox,
            'STARTPROPERTIES': lambda _: parse_properties(f),
            'CHARS': cmd_chars,
        }[cmd](arg)
    return (w, h), chars

def parse():
    with open('spleen-%dx%d.bdf' % CHAR_DIM) as f:
        cmd, w = next(f).split(' ', 1)
        (w, h), chars = {
            'STARTFONT': lambda _: parse_font(f),
        }[cmd](w)
    return (w, h), chars

(w, h), chars = parse()

ranges = []
prev = None
for k in sorted(chars):
    if prev == k - 1:
        ranges[-1][1].append(chars[k])
    else:
        ranges.append((k, [chars[k]]))
    prev = k

import os
from pathlib import Path
out_dir = Path(os.getenv('OUT_DIR', ''))

with open(out_dir / ('spleen-%dx%d.bin' % CHAR_DIM), 'wb') as f:
    f.write(len(ranges).to_bytes(4, 'little'))
    f.write(w.to_bytes(2, 'little'))
    f.write(h.to_bytes(2, 'little'))
    for start, v in ranges:
        end = start + len(v)
        f.write(start.to_bytes(4, 'little'))
        f.write(end.to_bytes(4, 'little'))
    for _, v in ranges:
        for c in v:
            assert len(c) == h
            x = 0
            for x in c:
                f.write(x.to_bytes(1, 'little'))
