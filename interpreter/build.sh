#!/bin/sh

set -xe

as main.s -o "$tmp/main.o"
ld -Tlink.ld -pie "$tmp/main.o" -o "$databin/init"
