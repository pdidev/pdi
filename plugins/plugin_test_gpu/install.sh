#!/bin/bash

# flags="-g3 -Wall -Wextra -Wno-unknown-pragmas -Wconversion -Wdouble-promotion"
flags="-g3 -Wall -Wextra -Wpedantic -Wno-unknown-pragmas -Wconversion -Wdouble-promotion"

bpath="build"
# [ ! -d $bpath ] && cmake -S . -B $bpath -DCMAKE_C_COMPILER=afl-cc -DCMAKE_CXX_COMPILER=afl-c++ -DCMAKE_C_FLAGS=$flags -DCMAKE_CXX_FLAGS=$flags
[ ! -d $bpath ] && cmake -S . -B $bpath -DBUILD_INDENT=ON -DBUILD_TESTING=ON -DCMAKE_C_FLAGS=$flags -DCMAKE_CXX_FLAGS=$flags
cmake --build $bpath -j || exit
# cmake --install $bpath

cmake --build build --target test
