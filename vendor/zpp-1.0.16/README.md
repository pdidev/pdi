# zpp

This is zpp, the Z Pre-Processor.

Zpp transforms bash in a pre-processor for F90 source files.
It offers a set of functions specifically tailored to build clean Fortran90
interfaces by generating code for all types, kinds, and array ranks supported by
a given compiler.

## Syntax

Zpp files are typically named `*.F90.zpp`.

In these files, the lines that start with `!$SH` are interpreted as bash lines. 
Other lines are copied as-is, except that variable substitution is operated as
in a double-quoted string, including bash commands `${VAR}` or `$(command)`.
If inside a bash control block (`if`, `for`, etc.), the output generation obeys
the control statement.

For example, this code:
```
!$SH for GREETED in world universe multiverse ; do
print *, "Hello ${GREETED}"
!$SH done
```

Would produce the following result:
```
print *, "Hello world"
print *, "Hello universe"
print *, "Hello multiverse"
```

### Support functions

Predefined bash functions, variable and code can be provided in `.zpp.sh` files
that can be included with `#!SH source <filename>.zpp.sh`.

**Beware**: a file NEEDs to have the `.zpp.sh` extension to be included from
zpp.

Zpp provides a standard library of functions tailored to build clean Fortran90
interfaces by generating code for all types, kinds, and array ranks supported by
a given compiler.

#### zpp_str_repeat

Found in `base.zpp.sh`

Outputs a string multiple times.

Parameters:
1. the string to Repeat
2. the lower bound of the iterations (inclusive)
3. the upper bound of the iterations (inclusive)
4. the string separator
5. the string starter
6. the string ender

Repeats string `$1` (`$3`-`$2`+1) times, separated by string `$4` inside `$5`
`$6`.
* If the number of repetitions is negative, the result is empty.
* If `$1` contains the '@N' substring, it will be replaced by the iteration
  number (from `$2` to `$3`).

example:
```
#!SH source base.zpp.sh
zpp_str_repeat v@N 5 7 '...' '<<' '>>'
zpp_str_repeat w@N 1 1 '...' '<<' '>>'
zpp_str_repeat x@N 1 0 '...' '<<' '>>'
```
output:
```
<<v5...v6...v7>>
<<w1>>
```

#### zpp_str_repeat_reverse

Found in `base.zpp.sh`

Outputs a string multiple times in reverse order.

Parameters:
1. the string to Repeat
2. the upper bound of the iterations (inclusive)
3. the lower bound of the iterations (inclusive)
4. the string separator
5. the string starter
6. the string ender

Repeats string `$1` (`$2`-`$3`+1) times, separated by string `$4` inside `$5`
`$6`.
* If the number of repetitions is negative, the result is empty.
* If `$1` contains the '@N' substring, it will be replaced by the iteration
  number (from `$2` to `$3`, i.e. upper to lower).

example:
```
#!SH source base.zpp.sh
zpp_str_repeat_reverse v@N 5 7 '...' '<<' '>>'
zpp_str_repeat_reverse w@N 1 1 '...' '<<' '>>'
zpp_str_repeat_reverse x@N 1 0 '...' '<<' '>>'
```
output:
```
<<v7...v6...v5>>
<<w1>>
```

#### ZPP_FORT_TYPES

Found in `fortran.zpp.sh`

The list of types supported by the fortran compiler as zpp:typeIDs.

The compiler ID should be provided in `ZPP_CONFIG` as `config.<ID>`.
The supported predefined IDs are: `Gnu`, `Intel`, `PGI` and `XL`.
You can also provide definitions for an additional compiler by defining
`ZPP_FORT_TYPES` in a file named `config.<ID>.zpp.sh`.

If you use cmake, it will automatically generate such a file for your compiler
and define  `ZPP_CONFIG` so you don't have to handle it.

### zpp_fort_array_desc

Found in `fortran.zpp.sh`

Outputs an assumed shaped array descriptor of the provided size.

Parameters:
1. the size of the assumed shaped array

example:
```
#!SH source fortran.zpp.sh
integer:: scalar$(zpp_fort_array_desc 0)
integer:: array1d$(zpp_fort_array_desc 1)
integer:: array2d$(zpp_fort_array_desc 2)
```
output:
```
integer:: scalar
integer:: array1d(:)
integer:: array2d(:,:)
```

### zpp_fort_ptype

Found in `fortran.zpp.sh`

Outputs the type associated to a zpp:typeID.

Parameters:
1. a zpp:typeID

example:
```
#!SH source fortran.zpp.sh
!$SH for T in ${ZPP_FORT_TYPES} ; do
$(zpp_fort_ptype $1)
!$SH done
```
example output:
```
CHARACTER
COMPLEX
COMPLEX
INTEGER
INTEGER
INTEGER
INTEGER
LOGICAL
REAL
REAL
```

### zpp_fort_kind

Found in `fortran.zpp.sh`

Outputs the kind associated to a zpp:typeID.

Parameters:
1. a zpp:typeID

example:
```
#!SH source fortran.zpp.sh
!$SH for T in ${ZPP_FORT_TYPES} ; do
$(zpp_fort_kind $1)
!$SH done
```
example output:
```
1
4
8
1
2
4
8
1
4
8
```

### zpp_fort_type

Found in `fortran.zpp.sh`

Outputs the full type (with kind included) associated to a zpp:typeID.

Parameters:
1. a zpp:typeID
2. additional attributes for the type

example:
```
#!SH source fortran.zpp.sh
!$SH for T in ${MY_CHAR_TYPES} ; do
$(zpp_fort_type $1)
$(zpp_fort_type $1 "len=5")
!$SH done
```
example output:
```
CHARACTER(KIND=1)
CHARACTER(KIND=1,len=5)
```

### zpp_fort_sizeof

Found in `fortran.zpp.sh`

Outputs the size in bits associated to a zpp:typeID.

Parameters:
1. a zpp:typeID

### zpp_fort_io_format

Found in `fortran.zpp.sh`

Outputs an IO descriptor suitable for a zpp:typeID.

Parameters:
1. a zpp:typeID

### ZPP_HDF5F_TYPES

Found in `hdf5_fortran.zpp.sh`

A list of zpp:typeIDs supported by HDF5.

### hdf5_constant

Found in `hdf5_fortran.zpp.sh`

Outputs the HDF5 type constant associated to a zpp:typeID.

Parameters:
1. a zpp:typeID

example:
```
#!SH source hdf5_fortran.zpp.sh
!$SH for T in ${ZPP_HDF5F_TYPES} ; do
$(hdf5_constant $1)
!$SH done
```
example output:
```
H5T_NATIVE_INTEGER
H5T_NATIVE_REAL
H5T_NATIVE_REAL
H5T_NATIVE_CHARACTER
```


## Command-line interface

Zpp basic usage is as follow:
```
Usage: zpp [Options...] <source> [<destination>]
  use `zpp -h' for more info

Preprocesses BASH in-line commands in a source file

Options:
  --version        show program's version number and exit
  -h, --help       show this help message and exit
  -I DIR           Add DIR to search list for source directives
  -o FILE          Place the preprocessed code in file FILE.
  -D OPTION=VALUE  Set the value of OPTION to VALUE
```

## CMake interface

Support is provided for using zpp from CMake based projects, but you can use
it from plain Makefiles too.

There are two ways you can use zpp from your CMake project:
* with `add_subdirectory`: include zpp in your project and use it directly from
  there,
* with `find_package`: install zpp and use it as an external dependency of your
  project.

#### CMake subdirectory usage

Using zpp with `add_subdirectory` is very simple.
Just copy the `zpp` directory in your source and point to it with
`add_subdirectory(zpp)`.
The `zpp_preprocess` then becomes available to process zpp files.

This is demonstrated in `example/cmake_subdirectory`.

#### CMake find usage

Using zpp with `find_package` is no more complex.
If zpp is installed, just add a `find_package(zpp REQUIRED)`.
The `zpp_preprocess` then becomes available to process zpp files.

This is demonstrated in `example/cmake_find`.

## GMake usage

Using zpp from a GNU Makefile is slightly less powerful than from CMake.
The types and kinds supported by the Fortran compiler will not be automatically
detected.
Predefined lists of supported types for well known compilers are provided
instead.

To use zpp from a Makefile, include the `share/zpp/zpp.mk` file (either from an
installed location or from a subdirectory in your project).
You can then set the `zpp_COMPILER_ID` variable to the compiler you use and
`.F90` files will be automatically generated from their `.F90.zpp` equivalent.
The `zppFLAGS` variable is automatically passed to zpp similarly to `CFLAGS` or
`CXXFLAGS` for `cc` and `cxx`.

This is demonstrated in `example/cmake_makefile`.

## Installation

Zpp can be installed using the usual python way with `setup.py`.
```
./setup.py --help
```

The cmake approach is deprecated.

## FAQ

Q. Isn't zpp redundant with assumed type parameters?

A.
The assumed type parameters functionality allows to implement part of what can
be done with zpp (support for all kinds of a type). However as of 2013 it was
not correctly supported on most compilers installed on the supercomputers.

In addition, many things can be done with zpp but not with assumed type
parameters, such as support for variable array rank or small variations of the
code depending on the kind.
