\page FAQ FAQ

# Compile PDI with Intel compiler

The Intel compiler is a complex beast for C++.
It does not provide a full compilation environment but relies on GNU C++
library.
Hence on a machine using environment modules (such as supercomputers), you
will typically need to load a GCC module before using Intel C++ compiler.
It is however fully compatible with the GNU compiler for C & C++.
Only the Fortran compiler uses a different module format.

\attention
If you don't use PDI from Fortran, we strongly advise to compile PDI with GCC
even if you compile the code that use PDI with Intel compiler.

Otherwise, to use PDI from a code compiled with IFort, you have 3 options (by
order of recommendation):
1. compile PDI with GCC for C/C++, only use Intel compiler for Fortran,
2. compile PDI in a fully GCC environment and load PDI with `include` instead
   of using the module,
3. compile PDI in a fully Intel environment, with GNU C++ library only.

## compile PDI with GCC for C/C++, use Intel compiler for Fortran

On a machine using environment modules you will need to load both GCC and Intel
modules.

Then, compile PDI by specifying which compiler you want to use for each language
to `cmake`.
You can of course combine these option with the usual `cmake` options.

```
CC=gcc CXX=g++ FC=ifort cmake -DCMAKE_INSTALL_PREFIX="${HOME}/.local/" ..
make install
```

Even if you don't use it to compile your code, you must keep the GCC module
loaded on a machine using environment modules.

## load PDI with `include` instead of using the module

## compile PDI in a fully Intel environment, with GNU C++ library only

On a machine using environment modules you will need to load both GCC and Intel
modules even  if we won't be using GCC at all.

Then, compile PDI normally, you might want to force the use of Intel compilers.
You can of course combine these option with the usual `cmake` options.

```
CC=icc CXX=icpc FC=ifort cmake -DCMAKE_INSTALL_PREFIX="${HOME}/.local/" ..
make install
```

Even if you don't use it to compile your code, you must keep the GCC module
loaded on a machine using environment modules.
