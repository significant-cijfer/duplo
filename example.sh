#!/bin/sh

mkdir -p dup-out

set -xe

zig build run -- example.dup > dup-out/example.c
cc dup-out/example.c lib.c -o dup-out/example

set +xe

dup-out/example; echo $?
