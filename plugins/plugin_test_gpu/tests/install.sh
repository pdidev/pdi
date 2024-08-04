#!/usr/bin/bash

bpath="build"

[ ! -d $bpath ] && cmake -S . -B $bpath
cmake --build build -j || exit