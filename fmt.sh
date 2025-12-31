#!/bin/sh

for f in qemubios runtime/*/* lib/* driver/* service/*
do
	echo formatting $f
	(cd $f && cargo fmt) &
done
wait
