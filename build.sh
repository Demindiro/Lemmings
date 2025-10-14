#!/bin/sh
set -xe
(cd rust && ./build.sh)
(cd qemubios && ./build.sh)
(cd kernel && ./build.sh)
