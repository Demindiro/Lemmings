#!/bin/sh

for f in qemubios runtime/*/* lib/* driver/*
do
	echo formatting $f
	(cd $f && cargo fmt)
done
