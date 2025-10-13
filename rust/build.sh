#!/bin/sh

. ../config.env
export RUSTC_BOOTSTRAP=1

set -xe

TARGET_SPEC="$PWD/../x86_64-unknown-lemmings.json"
for opt in '' '--release'
do
	for pkg in core compiler_builtins rustc-std-workspace-core
	do
		(cd "$RUST_SRC" && cargo b --package $pkg --target "$TARGET_SPEC" $opt)
	done
done
