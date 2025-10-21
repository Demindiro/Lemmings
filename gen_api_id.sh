#!/bin/sh
echo -n 0x
function f { hexdump -n4 -e '"%x"' /dev/urandom; }
function g { echo -n _; }
f; g; f; g; f; g; f
echo
