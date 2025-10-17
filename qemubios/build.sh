#!/bin/sh

. ../config.env
export RUSTC_BOOTSTRAP=1

set -xe

TARGET=x86_64-qemubios
RUST_TARGET="$RUST_OUT/$TARGET"
TARGET_SPEC="$PWD/$TARGET.json"

for pkg in core compiler_builtins rustc-std-workspace-core
do
	(cd "$RUST_SRC" && cargo b --release --package $pkg --target "$TARGET_SPEC" $opt)
done

rustc --edition=2024 --crate-type=lib \
	--target "$TARGET_SPEC" \
	-L"$RUST_TARGET/release" \
	-O \
	qemubios.rs -o /tmp/qemubios-rs.o

as qemubios.s -o /tmp/qemubios.o
ld /tmp/qemubios.o /tmp/qemubios-rs.o -T qemubios.ld -o /tmp/qemubios.bin
