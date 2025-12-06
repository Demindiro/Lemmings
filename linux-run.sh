#!/bin/sh

. ./config.env

set -xe
cd /tmp/data.bin.dir
exec "$RUST_OUT"/x86_64-unknown-linux-none/release/lemmings-runtime-linux-x86_64 "$@"
