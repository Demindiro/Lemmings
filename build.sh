#!/bin/sh
set -xe

. ./config.env

export RUSTC_BOOTSTRAP=1

export tmp="/tmp/lemmings.tmp"
export out="/tmp"
export databin="$out/data.bin.dir"

rm -rf "$databin"
mkdir -p "$databin"
mkdir -p "$tmp"

make -C IDL

(cd qemubios && cargo b --release -Zbuild-std=core --target ./x86_64-qemubios.json)
(cd runtime/kernel/x86_64 && ./build.sh)
(cd interpreter && ./build.sh)
(cd driver/virtio-net && cargo b --release -Zbuild-std=core)

cp interpreter/example.interpreter "$databin/interpreter.init"
mkdir -p "$databin/driver"
cp "$RUST_TARGET/release/lemmings-driver-virtio-net" "$databin/driver/virtio-net"

./create_archive.py "$out/data.bin" "$databin"
