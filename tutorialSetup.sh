#!/bin/bash

## This script is meant to be used from the pdi repository root
## Otherwise, you can uncomment the following two lines to get a full installation of pdi and its tutorial
# git clone git@github.com:pdidev/pdi.git
# cd pdi

git clone git@github.com:pdidev/tutorial.git -b new_2024
mkdir build
cd build
cmake ..
mkdir ../install
cmake -DCMAKE_INSTALL_PREFIX="../install" -DBUILD_PYCALL_PLUGIN=ON -DBUILD_PYTHON=ON -DUSE_HDF5="EMBEDDED" -DUSE_NetCDF="EMBEDDED" ..
make -j8
make install
source ./staging/share/pdi/env.sh

## Uncomment the following lines to test the setup via the resolution of ex10
# cd ../tutorial
# cmake .
# cp ./solutions/ex10.yml ./ex10.yml
# make ex10
# mpirun -np 4 ./ex10