#!/bin/sh

ghc --make bench.hs -O -o bench
echo Run 1
time ./bench
echo Run 2
time ./bench
echo Run 3
time ./bench
rm -f *.hi *.o bench
