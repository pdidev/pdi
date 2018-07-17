# Joining the PDI dev team

To make you known to others, please join the PDI slack channel at 
https://pdidev.slack.com

# Branching model

Each feature should be developed in answer to a requirement described in an
issue. Each feature should be implemented in its own branch based on the
`master` branch. WIP Merge requests should be created on gitlab early to discuss
design choices. Branches should be rebased on `master` before integration.

Bug fixes should be developed on the latest release branch. The release branch
should then be merged into master.

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

## Indentation

A astyle configuration file is provided with in `docs/indent.astyle`.
You can apply the formatting to the source in your working copy with the command
```
make indent
```
in your build directory.
