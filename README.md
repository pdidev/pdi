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
 * gcc
 
#### Additionnal functionnality requirement
Fortran language is supported (require a fortran compiler!).

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
