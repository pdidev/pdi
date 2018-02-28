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

* macro names *must* be all caps and begin with the `PDI_` prefix
* any public (app or plugin) symbol *must* be exported using the `PDI_EXPORT`
  macro

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
* any public or library-wide function *should* return a `PDI_status_t` error status

### For C++

Naming:
* class names *should* begin with an upper case and use underscore to separate words
* non-static member variables names *should* begin with an `m_` followed by lower case and underscores
* static member variables names *should* begin with an `s_` followed by lower case and underscores
* functions and variable names *should* begin with a lower score followed by underscores to separate words

File organization:
* public plugin enumerations *must* be defined in the `pdi/fwd.h` header
* public plugin classes and structures *must* be forward-declared in the
  `pdi/fwd.h` header

## Indentation

A astyle configuration file is provided with in `docs/indent.astyle`.
You can apply the formatting to the source in your working copy with the command
```
make doc
```
in your build directory.