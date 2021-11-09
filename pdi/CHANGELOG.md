# Changelog for PDI
All notable changes to the PDI library will be documented in this file.

Please note this is the list for the distribution mechanism of PDI. The list for
each sub-project (including PDI itself) is located in the dedicated sub-project
`CHANGELOG.md`.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]

### Added

### Changed

### Deprecated

### Removed

### Fixed

### Security



## [1.4.0] - 2021-11-09


### For users

#### Fixed
* Fixed a memory leak of Datatype object when data was reclaimed
  [#379](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/379)


### For plugin developers

#### Changed
* Logger getter in Context now returns a reference instead of pointer
  [#354](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/354)




## [1.3.0] - 2021-07-30


### For users

#### Added
* Improved error messages when failing to load a plugin
* support setting the logging format from the specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)




## [1.2.0] - 2021-06-15


### For users

#### Added
* Made error messages regarding yaml more informative by adding information
  about the location of the error in the file (line number)
  [#314](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/314)
* Support $-expression formatting using {fmt} https://github.com/fmtlib/fmt
  [#303](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/303)
* Support user-defined types aliases in specification file
  [#330](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/330)

#### Changed
* The `PDI_share` and `PDI_expose` subroutines of the Fortran API now accept
  non-pointer and non-contiguous data
  [#228](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/228)

#### Fixed
* Handle byte and unsigned integer arrays from Python API
  [#320](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/320),
  [#322](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/322)
* Fix a Fortran issue where in transactions, de-allocated temporary data copy
  could be referenced by PDI
  [#228](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/228)




## [1.0.1] - 2021-02-27

### For users

#### Fixed
* Handle the members in a yaml `struct` definition as an ordered mapping instead
  of a plain mapping.
* Fixed errors that would appear when using unsigned values (including Fortran
  logical) in $-expressions.




## [1.0.0] - 2021-01-28

### For users

#### Added
* Lookup for plugins in paths specified by the `PDI_PLUGIN_PATH` env. variable
* Install plugins as specified by the `INSTALL_PDIPLUGINDIR` cmake variable
* Improved error reporting when loading plugins
* Support for accessing record datatype from Python
* Support loading plugins in standard path relative to LD_LIBRARY_PATH
* Support for accessing members in $-expressions

#### Changed
* Fortran support now requires version 1.0.6 of Zpp minimum
* Python support now depends on python 3
* Require the CONTIGUOUS attribute for all Fortran arrays (including 1D)
* Default logger level for plugin is the logger level of PDI core

#### Fixed
* Added the CONTIGUOUS attribute to all Fortran arrays in tests


### For plugin developers

#### Added
* Expression arithmetic operators
* The `PDI_DEFAULT_PLUGINDIR` cmake variable provides the default plugin installation path
* Subaccess methods for array, record and pointer datatypes
* Ref operator[] for getting subref by index and member name
* Documentation of coupling PDI with user application
