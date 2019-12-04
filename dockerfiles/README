# Docker images:

## centos_base

The minimal dependencies to build the full PDI distribution except for cmake

## centos_cmake3.5

centos_base extended with the minimal dependencies to build the full PDI 
distribution with cmake-3.5 (without running the tests):
* cmake 3.5
* hdf5 because HDF5 embedded build requires cmake-3.10

Used for PDI embedded build with cmake 3.5 (HDF5 from system)

## centos_cmake3.10

centos_base extented with the minimal dependencies to build the full PDI 
distribution with cmake-3.10 and to run the tests:
* cmake 3.10
* numpy
* mpi4py

Used for PDI embedded build and tests with cmake 3.10 (all libs are embedded)

## centos_libs

Building of the dependencies required for the PDI distribution without relying
on embedded dependencies:
* libyaml,
* astyle,
* bpp,
* flowvr,
* libparaconf,
* fti,
* spdlog,
* pybind11,
* sionlib

## centos_libs_cmake3.5

Integration of the libraries from centos_libs into centos_cmake3.5

Used for PDI system build with cmake 3.5

## centos_libs_cmake3.10

Integration of the libraries from centos_libs into centos_cmake3.10.
In addition, hdf5 that is not available in centos_libs because it is in
centos_cmake3.5 is integrated.

Used for PDI system build and tests with cmake 3.10

# Docker images dependencies

```
centos_base -----> centos_cmake3.5 ----------------------> centos_libs_cmake3.5
           \                                           /
            \                      .-> centos_libs ---<
             \                    /                    \
              `-> centos_cmake3.10 ----------------------> centos_libs_cmake3.10
```
