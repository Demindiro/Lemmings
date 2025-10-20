#!/bin/sh
set -xe

. ./config.env
export tmp="/tmp/lemmings.tmp"
export out="/tmp"
export databin="$out/data.bin.dir"

rm -rf "$databin"
mkdir -p "$databin"
mkdir -p "$tmp"

(cd qemubios && ./build.sh)
(cd kernel && ./build.sh)

./create_archive.py "$out/data.bin" "$databin"
