# PDI: the Parallel Data Interface.

## Main features
PDI is a library to decouple simulation codes from IO concerns.
PDI supports existing IO libraries through a plugin system.
PDI declarative API makes it possible to share buffers manipulated in the application with IO libraries.
The type of data and the IO requests to forward to libraries are described in a dedicated YAML file.
PDI supports HDF5 and FTI as of now, other libraries are work-in-progress.


#### Existing plugins
* [x] HDF5 serial/one-file-per-process;
* [x] FTI
* [ ] ... see TODO.md


## Prerequisites

To build the library one needs:
  * cmake, version >= 3.1
  * a C compiler (gcc and icc are tested).
  * a MPI library

PDI also requires the BPP tool and Paraconf library that are distributed together with PDI in the `vendor` directory.
  * paraconf depends on libyaml. By default paraconf uses the system libyaml but it also embedds a copy that can be used by passing the `-DUSE\_SYSTEM\_YAML=OFF` option to cmake.

Fortran support:
  * a working Fortran compiler with 'iso_c_binding` support is required.

Plugins:
  * the HDF5 plugin require a version of HDF5 compatible with the chosen MPI (and Fortran compiler if enabled)
  * the FTI plugin depends on the FTI library that 
PDI can provide access to various libaries that should be included in the vendors directory. 
Thoses library can be disabled or remove if not required.
List of included optionnal vendors: FTI.


## Getting the source

As of now, the source has to be fetched from git .


```
git clone --recursive 
```

## Compilation

if the sources are in the folder pdi:

```
cd pdi
mkdir build
cd build
cmake .. 
make
make install
```
