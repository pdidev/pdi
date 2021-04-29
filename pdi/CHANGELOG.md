# Changelog for PDI
All notable changes to the PDI library will be documented in this file.

Please note this is the list for the distribution mechanism of PDI. The list for
each sub-project (including PDI itself) is located in the dedicated sub-project
`CHANGELOG.md`.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).




## [Unreleased]


### For users

#### Added
* Made error messages regarding yaml more informative by adding information
  about the location of the error in the file (line number)
* Support $-expression formatting using {fmt} https://github.com/fmtlib/fmt
* Support user-defined types aliases in specification file

#### Changed
* The `PDI_share` and `PDI_expose` subroutines of the Fortran API now accept
  non-pointer and non-contiguous data

#### Deprecated

#### Removed

#### Fixed
* Handle byte and unsigned integer arrays from Python API
* [Fortran] Fix an issue where in transactions, de-allocated temporary data copy
  might be referenced by PDI (issue #228)

#### Security


### For plugin developers

#### Added

#### Changed

#### Deprecated

#### Removed

#### Fixed

#### Security




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
