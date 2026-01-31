# Changelog for PDI
All notable changes to the PDI library will be documented in this file.

Please note this is the list for the distribution mechanism of PDI. The list for
each sub-project (including PDI itself) is located in the dedicated sub-project
`CHANGELOG.md`.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).



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



## [1.10.0] - 2026-01-31

### For users

#### Added
* Added macOS CI [#556](https://github.com/pdidev/pdi/issues/556)

#### Changed
* Update the version of dependencies according to our policy: oldest supported
  Debian, Fedora & Ubuntu, as well as spack 0.19. The new requirements are:
  CMake 3.22, Doxygen 1.9.1, pybind11 2.9.1, Python 3.10.6, and spdlog 1.9.2
  [#613](https://github.com/pdidev/pdi/issues/613)



## [1.9.3] - 2026-01-16

### For users

#### Fixed
* Updated embedded versions of zpp to fix a build issue with recent cmake
  [#631](https://github.com/pdidev/pdi/issues/631)
* Updated doxygen files to be compatible with recent versions of Doxygen
  [#629](https://github.com/pdidev/pdi/issues/629)



## [1.9.1] - 2025-05-07

### For users

#### Fixed
* Solve a compilation issue where latest compilers do not transitively define
  `uint8_t`, fix [#577](https://github.com/pdidev/pdi/issues/577)



## [1.9.0] - 2025-03-07

### For users

#### Added
* `PDI_share`, `PDI_expose` and `PDI_multi_expose` now accept `const` data
  [#553](https://github.com/pdidev/pdi/issues/553)


### For plugin developers

#### Fixed
* Aligned memory allocation using `operator new`, fixes
  [#550](https://github.com/pdidev/pdi/issues/550) 



## [1.8.3] - 2025-02-25

### For users

#### Fixed
* Correctlty detect python path when distutils is part of setuptools, fixes
  [#544](https://github.com/pdidev/pdi/issues/544)
* Support MacOSX in env.sh to correctly load PDI there, fixes
  [#540](https://github.com/pdidev/pdi/issues/540)



## [1.8.2] - 2025-02-20

### For plugin developers

#### Fixed
* Improve documentation generation, add sidepanel & fix repository URL, fixes
  [#521](https://github.com/pdidev/pdi/issues/521) and
  [#522](https://github.com/pdidev/pdi/issues/522)



## [1.8.1] - 2025-01-23

### For users

#### Fixed
* Stop claiming to support old cmake versions we do not
  [#507](https://github.com/pdidev/pdi/issues/507)
* Fixed website generation
  [#506](https://github.com/pdidev/pdi/issues/506)



## [1.8.0] - 2024-11-28

### For users

#### Changed
* Update the version of dependencies according to our policy: oldest supported
  Ubuntu & Fedora, oldstable debian & spack 0.18. The new requirements are:
  CMake 3.16, Python 3.8, numpy 1.17, pybind11 2.4
  - do not embed doxygen anymore
  - directly use vendored gtest, benchmark & ZPP
  [#465](https://github.com/pdidev/pdi/issues/465)

#### Fixed
* Fixed compilation with {fmt} version 11+
  [#456](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/456)


### For plugin developers

#### Added
* Create a method that dereferences a pointer type and returns the data as a new
  Reference.
  [#443](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/443)
* Add the function `scalar_assign` to `Ref_any` to ease setting the value of a
  scalar data buffer.
* Add the `Context::find()` method.
  [#445](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/445)
* Add the `pybind11::dtype to_python(const std::shared_ptr<const Scalar_datatype>& scalar_type)`
  helper function.

#### Changed
* Replaced the astyle based indentation by a clang-format one, fixes
  [#349](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/349)
* Renamed `Datatype_template_ptr` to `Datatype_template_sptr` that should now be
  used instead.

#### Deprecated
* The `Datatype_template_ptr` type is now deprecated, `Datatype_template_sptr`
  should be used instead.



## [1.7.0] - 2024-07-17

### For users

#### Fixed
* Fix unexpected behavior with zsh where env variables were set incorrectly.
  Exit script if shell is unsupported
  [#434](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/434)
* Fixed compilation with Python version > 3.12
* Stop using PyBind11 internals to support latest releases.
  [#448](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/448)


### For plugin developers

#### Added
* Create a method that dereferences a pointer type and returns the data as a new
  Reference.
  [#443](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/443)



## [1.6.0] - 2023-03-08

### For users

#### Changed
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
* The `PDI_DEFAULT_PLUGINDIR` cmake variable provides the default plugin
  installation path
* Subaccess methods for array, record and pointer datatypes
* Ref operator[] for getting subref by index and member name
* Documentation of coupling PDI with user application
