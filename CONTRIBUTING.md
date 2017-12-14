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
* member functions names *should* begin with a lower score followed by underscores to separate words

Visibility:
* public plugin enumerations *must* be defined in a header ending with the
  `_fwd.h` suffix
* public plugin classes, structures *must* be declared in a header ending with
  the `_fwd.h` suffix and defined in a file with the same name except the
  `_fwd` part
* functions *must* be declared in headers without any `_fwd` part

## Indentation

A astyle configuration file is provided with a Makefile in
`/tools/Format_and_Indent/`:
* `make indent_all`: copy and create indented copies into the subfolder
  workdir
* `make replace_all`: replace the original sources and headers with the
  indented versions from workdir. __  _If the original sources files were
  modified, this will replace your files!_ __
* `make all` / `make` : execute the two previous commands.
* `make clean`: remove the temporary content of workdir

# Gitlab: new branch, merge request, issues

* Branch names, merge request and issues titles consist of three zones
	* 1st, type of improvement: 
		- bugfix : remove a bug
		- feature : add a new feature to PDI or an existing plug-in
		- update : update an existing feature
		- new : add a new plug-in
		- ... etc.
	* 2nd, part of the software concerned: PDI, one plug-in, all plug-ins...
	* Last, a brief description 
	* Examples:
		- bugfix--HDF5_per_process--Something_is_working_better_there
		- feature--PDI--Now_make_the_tea
* Each zone is delimited by two hyphens
* Spaces are replaced by underscores
