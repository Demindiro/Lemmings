#!/bin/sh

. ./config.env

set -xe
"$RUST_OUT"/x86_64-unknown-linux-none/release/lemmings-runtime-linux-x86_64
