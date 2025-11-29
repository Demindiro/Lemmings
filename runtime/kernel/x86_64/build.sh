#!/bin/sh

set -xe

. ../../../config.env

export RUSTC_BOOTSTRAP=1
export __CARGO_TESTS_ONLY_SRC_ROOT="$RUST_SRC"
cargo b --release --target ../../../x86_64-unknown-lemmings.json -Zbuild-std=core
