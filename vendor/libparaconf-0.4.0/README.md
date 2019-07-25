# Paraconf distribution

[Paraconf](paraconf/) is a library that provides a simple query language to
access a Yaml tree on top of [libyaml](https://pyyaml.org/wiki/LibYAML).

The paraconf distribution provides a single packages with paraconf, its
dependancies and examples.

## Getting the source

You can find a list of release at 
https://gitlab.maisondelasimulation.fr/jbigot/libparaconf/tags

For example, you can get release 0.4.0:
```
wget https://gitlab.maisondelasimulation.fr/jbigot/libparaconf/-/archive/0.4.0/libparaconf-0.4.0.tar.bz2
tar -xjf libparaconf-0.4.0.tar.bz2
mv libparaconf-0.4.0 libparaconf
```


## Compilation

if the sources are stored in the folder `libparaconf`:
```
cd libparaconf
cmake \
        -DUSE_YAML=EMBEDDED \
        -DCMAKE_INSTALL_PREFIX=/usr/ \
        .
make install
```

## Prerequisites

The paraconf distribution depends on:
  * cmake, version >= 3.5
  * a C-99 compiler (gcc-5.4 is tested)
  * a POSIX compatible OS (linux with GNU libc-2.27 is tested)
  * [libyaml](https://pyyaml.org/wiki/LibYAML) (distributed with Paraconf
    distribution, pass the `-DUSE_YAML=EMBEDDED` option to cmake to use the
    embedded version)

Paraconf Fortran support depends on:
  * a Fortran-2003 compiler (gfort-5.4 is tested)

## Usage

Look at the [paraconf/](paraconf/) directory to some basic usage documentation.
Look at the [example/](example/) directory to get an example of usage.
