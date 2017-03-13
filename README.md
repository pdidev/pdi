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


## Installation
Currently, to obtain the library one needs to build it from source.
Pre-build binaries are not available.

### Prerequisites
#### Minimum requirement
To build the library one needs:
 * cmake, version >= 3.1
 * mpi  
 * a C compiler (e.g, *icc*, *gcc*, ...).

Currently, PDI requires BPP and Paraconf vendors that are located in vendor directory. See Vendors hereafter.
 
#### Vendors
PDI library have mandatory dependancies that are shipped with it:
  * Paraconf, a wrapped of the *YAML* C library that provide a C and Fortran API. By default Paraconf uses the system YAML library. This can be desactivated with the cmake option -DUSE\_SYSTEM\_YAML=OFF.
  * BPP, a **B**ash **P**re-**p**rocessor that expends lines of code using Bash directives.

#### Optionnal: 
  * **Fortran compatibility** : 
Fortran language is supported. This requires a Fortran compiler, for instance *gfortran, ifort*.

  * **Additional vendors** :
PDI can provide access to various libaries that should be included in the vendors directory. 
Thoses library can be disabled or remove if not required.
List of included optionnal vendors: FTI.


## Get the source

2 options:
* get a release,
* get the latest source from Git

### Get a release

Go to page ???

### From git

```
git clone --recursive 
```

## Step by step
if the sources are in the folder pdi:


```
cd pdi
mkdir build
cd build
cmake .. 
make
make install
```

# Usage
