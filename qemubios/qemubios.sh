#!/bin/sh

set -xe

as qemubios.s -o /tmp/qemubios.o
ld /tmp/qemubios.o -T qemubios.ld -o /tmp/qemubios.bin
