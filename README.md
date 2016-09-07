# PDI: the Parallel Data Interface.

## Main features
PDI is a C library to manage how data are imported and exported.
An API is provided whose generic interface handles data IO and format.
PDI uses existing libraries that are integrated to PDI through plugins.
Before running a program that uses PDI a configuration file should be provided which describes which plugins are used and how.
The library currently supports HDF5, FTI format (and soon many other ;) ).


#### Existing plugins
* Export
    * [ ] HDF5
    	* [x] HDF5 serial/one-core-one-file;
    	* [ ] HDF5 parallel
	* [ ] AH5 (HDF5 asynchronous)
    * [x] FTI
    * [ ] ... see TODO.md
* Import

All the importers are under developing.


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

