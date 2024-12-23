# Joining the PDI dev team

To make you known to others, please join the PDI slack channel at 
https://pdidev.slack.com

# Branching model

Each feature should be developed in answer to a requirement described in an
issue. Each feature should be implemented in its own branch based on the
`main` branch. Draft pull requests should be created on github early to discuss
design choices. Branches should be rebased on `main` before integration.

Bug fixes should be developed on the latest release branch. The release branch
should then be merged into main.

# PDI Coding style

## API levels

There are four levels of API for PDI:

| **API level**       | **Headers**               | **Language** |
|:-------------------:|:-------------------------:|:------------:|
|  public application | include/pdi.h             | C            |
|  public application | include/pdi.F90           | Fortran      |
|  public plugin      | include/pdi/*.h.          | C++          |
|  library-wide       | src/*.h                   | C++          |
|  file-local APIs    | not exposed in any header | C++          |

## Common C / C++ conventions

Naming:
* macro names *must* be all caps and begin with the `PDI_` prefix
* any public (app or plugin) symbol *must* be exported using the `PDI_EXPORT`
  macro

Includes *should* be grouped in blocks separated by an empty lines with files in
each block ordered in an alphabetical fashion.
All includes should use the bracket notation except for PDI header included
inside implementation (cxx) files.
The blocks *should* be in order:
1. in case of an implementation file (cxx), "config.h"
2. the "mpi.h" file
3. standard C/C++ includes
4. system (POSIX) headers
5. external library dependencies headers
6. PDI dependencies (starting with "pdi/pdi_fwd.h" for header files)
7. in case of an implementation file (cxx), the implemented PDI header

## C coding conventions

These conventions only apply to the C part of PDI (the C public application API)

Naming:
* all public elements (symbol, type, macro, ...) *must* start with the `PDI_`
  prefix
* for other elements the name *must* be lowercase except for the PDI prefix
  and *should* be constituted of words separated by underscores (snake_case)
* enum names *must* end with a `_e` suffix
* struct names *must* end with a `_s` suffix
* typedefs names  *must* end with a `_t` suffix

Prototypes:
* any public or library-wide function *should* return a `PDI_status_t` error
  status

## C++ coding conventions

The auto keyword should be used for types as `auto&&` and only in one of the
following cases:
* in iterations either in range based for or to refer to iterators
* to store objects that can not be typed otherwise (e.g. lambdas)
* to store the result of an expression specifically specifying the type of the
  generated value (e.g. a new expression)

Naming:
* class names *must* begin with an upper case letter and use underscore to
  separate words
* functions and variable names *must* begin with a lower case letter and use
  underscore to separate words
* non-static member variables names *should* begin with an `m_` followed by
  lower case and underscores
* static member variables names *should* begin with an `s_` followed by lower
  case and underscores
* global variables names *should* begin with an `g_` followed by lower case and
  underscores

File organization:
* public plugin enumerations *must* be defined in the `pdi/pdi_fwd.h` header
* public plugin classes and structures *must* be forward-declared in the
  `pdi/fwd.h` header
* No implementation should be provided in header files, even `= default`
  implementations

## Logging

Logging should be done using the spdlog library and exceptions. The standard C++
`cerr`, `clog` and `cout` should never be used except to diagnose issues that
appear before or after the initialization of spdlog.

The log levels should be used as follow, the first category that applies should
be used:
* **fatal error** an invalid situation has been detected by the library (invalid
  user input, invalid hardware behaviour, etc.), the library has no way to
  recover. This is reported by throwing an exception.
* **error** an invalid situation has been detected by the library (invalid
  user input, invalid hardware behaviour, etc.), the library will work-around
  this, but the user must definitely correct this.
* **warning** a very likely invalid situation has been detected by the library
  (user input that is technically valid, but very unusual for example), this is
  not technically incorrect so the library will continue as planned, but the
  user should definitely be notified.
* **info** a normal situation of the execution is likely useful for the user to
  understand the behaviour of the library after the fact, this should be limited
  to a few important steps of the execution. This should not include or
  require any knowledge about the internal working of the library.
* **debug** a normal situation of the execution might be useful for the user to
  understand the behaviour of the library when specifically looking to diagnose
  an unexpected behaviour. For example, a default value is used because the user
  didn't provide a specific value for a given call. This should not include or
  require any knowledge about the internal working of the library.
* **trace** a normal situation of the execution might be useful for the
  developers to diagnose the behaviour of the library. This typically requires
  knowledge about the internal working of the library to be correctly
  understood.

## Indentation

A `.clang-format` configuration file is provided at the root of PDI
distribution, the formatting it describes should be followed.
