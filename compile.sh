#!/bin/sh

./py2llvm.py simplebs.py > bs.ll
llvm-as bs.ll -f
opt -std-compile-opts bs.bc -o bs.opt.bc -f 
# llc bs.opt.bc -f
llc -march=c bs.opt.bc -f
gcc -g -msse2 main.c bs.opt.cbe.c -lm
