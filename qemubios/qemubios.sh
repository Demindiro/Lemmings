#!/bin/sh

set -xe

rustc --edition=2024 --crate-type=lib \
	-O \
	qemubios.rs -o /tmp/qemubios-rs.o

as qemubios.s -o /tmp/qemubios.o
ld /tmp/qemubios.o /tmp/qemubios-rs.o -T qemubios.ld -o /tmp/qemubios.bin
