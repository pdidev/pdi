#!/bin/bash

mkdir deisa && cd deisa
python3 -m venv deisa_python_env
source deisa_python_env/bin/activate
git clone https://github.com/pdidev/deisa.git
cd deisa
pip install .
cd ../..

git clone git@github.com:pdidev/pdi.git
cd pdi

git clone git@github.com:pdidev/tutorial.git -b new_2024
mkdir build
cd build
cmake ..
mkdir ../install
cmake -DCMAKE_INSTALL_PREFIX="../install" -DBUILD_PYCALL_PLUGIN=ON -DBUILD_PYTHON=ON -DBUILD_DEISA_PLUGIN=ON -DUSE_HDF5="EMBEDDED" -DUSE_NetCDF="EMBEDDED" ..

make -j8
make install
source ./staging/share/pdi/env.sh

cd ../tutorial
cmake .

mkdir ./ex_deisa/build
cd ./ex_deisa/build
cmake ..
cp ../../solution/deisa.yml ../../deisa.yml
make deisa

# # ---

# # Uncomment the following lines to check ex9
# cd ../..
# cp ./solutions/ex9.yml ./ex9.yml
# make ex9
# mpirun -np 4 ./ex9

# # Uncomment the following lines to check ex10
# cd ../..
# cp ./solutions/ex10.yml ./ex10.yml
# make ex10
# mpirun -np 4 ./ex10

# # Uncomment the following lines to check ex11
# cp ./solution/deisa.yml ./deisa.yml
# make deisa
# ./launch.sh