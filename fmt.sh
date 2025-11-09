#!/bin/sh

echo formatting qemubios
rustfmt qemubios/qemubios.rs

for f in kernel lib/* driver/*
do
	echo formatting $f
	(cd $f && cargo fmt)
done
