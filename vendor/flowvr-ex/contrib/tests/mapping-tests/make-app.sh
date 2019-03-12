#!/bin/sh


# simple script to configure, compile and install the example. 
PWD1=$PWD;
mkdir build;
cd build;
cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_VERBOSE_MAKEFILE=1 -DBUILD_MODULES=1  -DCMAKE_INSTALL_PREFIX:PATH=$PWD1 $PWD1;
make install;
make test;
cd $PWD1;
