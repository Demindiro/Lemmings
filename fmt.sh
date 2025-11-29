#!/bin/sh

for f in qemubios kernel lib/* driver/*
do
	echo formatting $f
	(cd $f && cargo fmt)
done
