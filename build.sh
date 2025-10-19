#!/bin/sh
set -xe
(cd qemubios && ./build.sh)
(cd kernel && ./build.sh)
./create_databin.sh
