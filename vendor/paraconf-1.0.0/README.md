# Paraconf distribution

[Paraconf](paraconf/) is a library that provides a simple query language to
access a Yaml tree on top of [libyaml](https://pyyaml.org/wiki/LibYAML).

The paraconf distribution provides a single packages with paraconf, its
dependancies and examples.

## Getting the source

You can find a list of release at 
https://github.com/pdidev/paraconf/releases

For example, you can get release 1.0.0:
```
wget https://github.com/pdidev/paraconf/archive/1.0.0.tar.gz
tar -xzf 1.0.0.tar.gz
mv paraconf-1.0.0 paraconf
```


## Compilation

if the sources are stored in the folder `paraconf`:
```
cd paraconf
cmake \
        -DCMAKE_INSTALL_PREFIX=/usr/ \
        .
make install
```

Change `-DCMAKE_INSTALL_PREFIX=/usr/` to something else if you want to install
somewhere else than in `/usr/`.

## Prerequisites

The paraconf distribution depends on:
  * cmake, version >= 3.5
  * a C-99 compiler (gcc-5.4 is tested)
  * a POSIX compatible OS (linux with GNU libc-2.27 is tested)
  * [libyaml](https://pyyaml.org/wiki/LibYAML) (included in Paraconf
    distribution)

Paraconf Fortran support depends on:
  * a Fortran-2003 compiler (gfort-5.4 is tested)

## Usage

Look at the [paraconf/](paraconf/) directory to some basic usage documentation.
Look at the [example/](example/) directory to get an example of usage.
