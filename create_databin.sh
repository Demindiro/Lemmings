#!/bin/sh

. ./config.env
o=/tmp/data.bin.dir

set -xe

rm -rf "$o"
mkdir -p "$o"

./create_archive.py /tmp/data.bin "$o"
