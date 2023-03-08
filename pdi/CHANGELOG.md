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

#### Changed

#### Deprecated

#### Removed

#### Fixed

#### Security


### For plugin developers

#### Added

#### Changed

#### Deprecated

#### Removed

#### Fixed

#### Security



## [1.6.0] - 2023-03-08

### For users

#### Changed
* Updated minimum required versions for paraconf to 0.4.16.
* Remove {fmt} as a direct dependency, use that provided by spdlog to fix
  [#431](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/431)
  (reverts
  [#413](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/413))
* Update required version of paraconf to 1.0.0



## [1.5.5] - 2022-11-03

### For users

#### Fixed
* Fixed an issue where operations would use the wrong type
  [#430](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/430)



## [1.5.2] - 2022-04-22

### For users

#### Fixed
* Fixed an issue when using PDI without C enabled from CMake
  [#423](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/423)



## [1.5.1] - 2022-04-01

### For users

#### Fixed
* Fixed memory leak when created Ref couldn't acquire read or write lock.
  [#418](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/418)



## [1.5.0] - 2022-03-30

### For users

#### Added
* Support for tuple datatype, an array-like structure where elements can be of
  different types.
  [#364](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/364)
* Support for size_t, ptrdiff_t and byte types.
  [#351](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/351)
* Support for string boolean types in yaml.
  [#362](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/362)
* Support for integers from stdint.h
  [#389](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/389)
* Improved error message when a share/release/reclaim fails
  [#410](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/410)
* Plugins can now add independent information to the PDI logger.
  [#360](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/360)

#### Changed
* Updated dependencies, now require cmake-3.10, paraconf-0.4.15, spdlog-1.5.0,
  python-3.6 if enabled and zpp-1.0.15 if Fortran is enabled.
* Updated the fortran interface with assumed type and assumed rank
  arguments (Fortran 2008) where possible

#### Fixed
* Add {fmt} as a direct dependancy to fix
  [#413](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/413)


### For plugin developers

#### Added
* Expression to_ref refactored to use copy_value method
  [#270](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/270)

#### Changed
* Datatype must now be used through shared_ptr

#### Removed
* Removed `Datatype::Accessor` and use simple virtual functions instead
  [#385](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/385)



## [1.4.2] - 2021-11-30

### For users

#### Fixed
* Fixed a bug where sparse array data was not cast correctly to Python.
  [#382](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/382)




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
