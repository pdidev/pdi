# Changelog for PDI
All notable changes to the PDI project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).



## [Unreleased]

### For users

#### Added
* A timer is added to reflect the time consumed by plugins.

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



## [1.11.1] - Unreleased

### For users

#### Added

#### Changed

#### Deprecated

#### Removed

#### Fixed
* Do not fail building the documentation when Fortran support is disabled, to
  fix [#690](https://github.com/pdidev/pdi/issues/690)

#### Security


### For plugin developers

#### Added

#### Changed

#### Deprecated

#### Removed

#### Fixed

#### Security



## [1.11.0] - 2026-04-22

### For users

#### Added
* Add subfiling support in Decl'HDF5
  [#602](https://github.com/pdidev/pdi/issues/602)
* Enable plugin `decl_netcdf` in macOS CI
* Added support for HDF5-2 by fixing an issue that arose when combining 
  cmake < 4.3 with HDF5 >= 2 [#678](https://github.com/pdidev/pdi/issues/678)
* added a `ENABLE_BENCHMARKING` flag to cmake to enable running the benchmarks
  as part of the tests (off by default)
  [#679](https://github.com/pdidev/pdi/issues/679)
* Add native netcdf deflate support with chunking 
  [#603](https://github.com/pdidev/pdi/issues/603)
* Add type check in Decl'NetCDF when reading scalar variable from file
  [#647](https://github.com/pdidev/pdi/issues/647)
* Added support for HDF5-2 by fixing an issue that arose when combining 
  cmake < 4.3 with HDF5 >= 2 [#678](https://github.com/pdidev/pdi/issues/678)

#### Changed
* The minimum version of C required is now C17 (ISO/IEC 9899:2018) instead of
  C11.
* Fully qualify `std::move` calls to prevent a compilation warning and incorrect
  usages [#675](https://github.com/pdidev/pdi/issues/675)
* benchmarks are not run as part of the test suite by default anymore, one must
  set `ENABLE_BENCHMARKING` to `ON` in Cmake to re-enable them

#### Deprecated
* Error names have been improved to fix
  [#670](https://github.com/pdidev/pdi/issues/670):
  - `PDI_ERR_CONFIG` has been renamed to `PDI_ERR_SPECTREE`,
  - `PDI_ERR_RIGHT` has been renamed to `PDI_ERR_PERMISSION`,
  - `PDI_UNAVAILABLE` is never used and will be removed.

#### Removed
* Removed the unmaintained benchmarking analysis tools

#### Fixed
* fix macOS Python support. [#656](https://github.com/pdidev/pdi/issues/656)
* Fix some incorrect uses of `{fmt}` that could lead to compilation errors in
  C++ 20+ or crashes in C++ <20 [#660](https://github.com/pdidev/pdi/issues/660)
* Fixed a rare bug, where the Decl'HDF5 plugin would crash when reporting an
  error with multiple matching regexes
  [#668](https://github.com/pdidev/pdi/issues/668)


### For plugin developers

#### Added
* Added a new testing API (`pdi/testing.h`) to replace usage of assert in plugin
  tests [#236](https://github.com/pdidev/pdi/issues/236)

#### Changed
* Versions of C++ used in now C++ 20 instead of C++ 17.
* `PDI::Right_error` has been replaced by `PDI::Permission_error`.
* `PDI::Config_error` has been replaced by `PDI::Spectree_error`.

#### Removed
* `PDI::Unavailable_error` was never used and has been removed.



## [1.10.1] - 2026-02-04

### For users

#### Added
* Added tests for using an installed PDI

#### Fixed
* Support multiple consecutive calls to `find_package(PDI)`
  [#526](https://github.com/pdidev/pdi/issues/526)



## [1.10.0] - 2026-01-31

### For users

#### Added
* Added macOS CI [#556](https://github.com/pdidev/pdi/issues/556)
* Add "mock PDI", to allow disabling PDI  while keeping code syntax unchanged
  [#438](https://github.com/pdidev/pdi/issues/438)
* Added tests for PDI API
* Possibility to add a regex for dataset name in Decl'HDF5 datasets section
  [#582](https://github.com/pdidev/pdi/issues/582)

#### Changed
* Update the version of dependencies according to our policy: oldest supported
  Debian, Fedora & Ubuntu, as well as spack 0.19. The new requirements are:
  CMake 3.22, Doxygen 1.9.1, HDF5 1.10.7, mpi4py 3.1.3, NetCDF 4.8.1,
  numpy 1.21.5, pyyaml 5.4.1, pybind11 2.9.1, Python 3.10.6, and spdlog 1.9.2
  [#613](https://github.com/pdidev/pdi/issues/613)

#### Removed
* Removed the Deisa plugin that does not match any current version of Deisa

#### Fixed
* Fix an issue where github action wouldn't run due to permission issue
  [#585](https://github.com/pdidev/pdi/issues/585)
* Fix a build error in libyaml with recent cmake versions
  [#593](https://github.com/pdidev/pdi/issues/593)
* Fix macOS linking issue when installing via root CMakeLists.txt
  [#565](https://github.com/pdidev/pdi/issues/565)
* Fix chunking test in decl_hdf5
  [#588](https://github.com/pdidev/pdi/issues/588)
* Fix data validation in decl_hdf5 test after write operation
  [#587](https://github.com/pdidev/pdi/issues/587)
* Fix default CMAKE_BUILD_TYPE value
  [#617](https://github.com/pdidev/pdi/issues/617)
* Fix HDF5 compression test for MacOS 26
  [#627](https://github.com/pdidev/pdi/issues/627)
* fix HDF5 API version compatibility issue during error handling
  [#567](https://github.com/pdidev/pdi/issues/567)
* Fixed a error incorrectly raised when using the scalar format in Decl'NetCDF
  yaml write configuration [#636](https://github.com/pdidev/pdi/issues/636)
* Fixed some tests in the Set-value plugin that would never fail, even in case
  of error



## [1.9.3] - 2026-01-16

### For users

#### Fixed
* Updated doxygen files to be compatible with recent versions of Doxygen
  [#629](https://github.com/pdidev/pdi/issues/629)
* Updated embedded versions of zpp , paraconf & pybind11 to fix a build issue
  with recent cmake [#631](https://github.com/pdidev/pdi/issues/631)


## [1.9.2] - 2025-06-13

### For users

#### Fixed
* Released 1.9.2 because 1.9.1 was incorrectly tagged



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
* Added new JSON plugin. Enables users to write data in JSON format, 
  using the [nlohmann/json](https://github.com/nlohmann/json) library.
  Users can also write data in other format thanks to taking JSON as input for
  the dedicated tools.
  [#440](https://github.com/pdidev/pdi/issues/440)
* Added the possibility to choose parallel MPI-I/O mode: either COLLECTIVE or
  INDEPENDENT
  [#419](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/419)

#### Fixed
* Fix known MacOS compile & execution issues 
  [#562](https://github.com/pdidev/pdi/issues/562)
* Fix macOS compile issue with the user-code plugin 
  [#539](https://github.com/pdidev/pdi/issues/539)


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
* Fix the indentation check script
  [#515](https://github.com/pdidev/pdi/issues/515)



## [1.8.0] - 2024-11-28

### For users

#### Added
* NetCDF plugin now support the size_of attribute, fixes
  [#446](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/446)
* Deisa plugin for in-situ analysis using Dask

#### Changed
* Update the version of dependencies according to our policy: oldest supported
  Ubuntu & Fedora, oldstable debian & spack 0.18. The new requirements are:
  CMake 3.16, NetCDF 4.7, Python 3.8, mpi4py 3.0, numpy 1.17, pybind11 2.4
  - do not embed doxygen anymore
  - directly use vendored gtest, benchmark & ZPP
  [#465](https://github.com/pdidev/pdi/issues/465)
* Replaced the astyle based indentation by a clang-format one, fixes
  [#349](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/349)

#### Removed
* Removed deprecated, unmaintained and unsupported plugins and tools:
  Decl'SION, FlowVR, FTI and test plugins, as well as the PDI Config validator,
  fixes [#458](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/458)

#### Fixed
* Fixed compilation with {fmt} version 11+
  [#456](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/456)
* Support installation with the latest version of python where setuptools is an
  external lib, fixes
  [#457](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/457)


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



## [1.7.1] - 2024-07-21

### For users

#### Fixed
* Fix the installation of embedded PyYAML, fixes
  [#455](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/455)



## [1.7.0] - 2024-07-17

### For users

#### Fixed
* Fixed compilation with Python version > 3.12
* Fix unexpected behavior with zsh where env variables were set incorrectly.
  Exit script if shell is unsupported
  [#434](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/434)
* Stop using PyBind11 internals to support latest releases.
  [#448](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/448)
* Updated the embedded version of PyYAML to 6.0.1 to fix an incompatibility with
  cython 3, fixes
  [#439](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/439)
* Update PDI tested versions of CMake from 3.10 to 3.25.
  Fixed CMake policy warnings introduced in v3.24 and v3.27.6, fixes
  [#437](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/437)


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
* NetCDF 4.6.2 is now required for parallel NetCDF support.
* Updated vendored PyYaml version to 6.0.

#### Fixed
* Correctly delete files when using parallel HDF5 to prevent a crash
  [#428](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/428)



## [1.5.5] - 2022-11-03

### For users

#### Fixed
* Fixed an issue where operations would use the wrong type
  [#430](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/430)
* Fixed a missing header compilation issue in the Serialization plugin with the
  latest GCC
  [#426](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/426)



## [1.5.4] - 2022-06-11

### For users

#### Fixed
* Correctly handle the case where HDF5 was already searched and not found
  [#427](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/427)



## [1.5.2] - 2022-04-22

### For users

#### Fixed
* Fixed an issue when using PDI without C enabled from CMake
  [#423](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/423)
* Fixed an issue where PDI would not build with cmake 3.23+
  [#422](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/422)



## [1.5.1] - 2022-04-01

### For users

#### Fixed
* Fixed memory leak when created Ref couldn't acquire read or write lock.
  [#418](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/418)
* Fixed an issue in the MPI plugin where transtyping would fail when the size of
  the integer used for the Fortran size is different from that of a Fortran
  integer:
  [#293](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/293)



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
* PDI benchmarks using google benchmark
  [#341](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/341)
* Support tuple datatype serialization
  [#364](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/364)
* Support for changing logger level and pattern during execution.
  [#363](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/363)

#### Changed
* Updated dependencies, the minimum required versions are now: 3.10 for cmake,
  1.6 for FTI, 1.10 for HDF5, 4.6 for NetCDF, 0.4.15 for paraconf, 3.6 for
  Python, 1.7.6 for sionlib, 1.5 for spdlog, and 1.0.15 for zpp.
* Force the `CMAKE_POSITION_INDEPENDENT_CODE` & `BUILD_SHARED_LIBS` to on for
  all sub-projects
* Updated the fortran interface with assumed type and assumed rank
  arguments (Fortran 2008) where possible
* Indent if required by the user, not automatically whenever astyle is found.

#### Fixed
* Add {fmt} as a direct dependancy to fix
  [#413](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/413)
* Fix an issue where libYaml is not found correctly when installed in a
  non-standard directory
  [#398](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/398)
* Fixed an issue where OpenGL and GLU would not be used correctly if installed
  in a non-standard path
  [#405](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/405)
* Fixed an issue where NetCDF would not be detected
  [#412](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/412)
* Fixed an issue where parallel NetCDF would not be detected correctly with
  NetCDF too old and the wrong tests would be run
  [#383](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/383)
* Do not run `Test05_C` when compiled without Decl'NetCDF support
  [#401](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/401)


### For plugin developers

#### Added
* Expression to_ref refactored to use copy_value method
  [#270](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/270)

#### Changed
* Datatype must now be used through shared_ptr

#### Removed
* Removed `Datatype::Accessor` and use simple virtual functions instead
  [#385](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/385)



## [1.4.3] - 2021-11-30

### For users

#### Fixed
* Fix an issue where HDF5 installed with cmake would not be correctly detected
  [#356](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/346)



## [1.4.2] - 2021-11-30

### For users

#### Fixed
* Fixed a bug where sparse array data was not cast correctly to Python.
  [#382](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/382)




## [1.4.1] - 2021-11-10

### For users

#### Changed
* updated FindHDF5 to a backport from CMake 3.21.

#### Fixed
* fixed a bug where macro definitions imposed by NetCDF would lead to duplicate
  `-D`, hence preventing compilation.



## [1.4.0] - 2021-11-09

### For users

#### Fixed
* Fixed a memory leak of Datatype object when data was reclaimed
  [#379](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/379)
* Initial value of `fti_head` was not defined in Fortran example. This could
  lead to timeout of Fortran example runs.
  [#369](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/369)
* Removed (unsupported by Clang < 11.0) default argument from function parameter
  before parameter pack.
  [#370](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/370)
* Corrected an unnecessary linear increase in heap size. The trace plugin now
  uses internal PDI callback to know when data is no longer shared.
  [#380](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/380)


### For plugin developers

#### Changed
* Logger getter in Context now returns a reference instead of pointer
  [#354](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/354)



## [1.3.0] - 2021-07-30

### For users

#### Added
* Improved error messages when failing to load a plugin
* Support type attributes in the Serialization plugin
* support setting the logging format from the specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)
* Support specifying a policy when the file or dataset to write to already
  exists in Decl'HDF5
  [#121](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/121)
* Support for dataset chunking, deflate and fletcher32 filters in Decl'HDF5
  [#324](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/324)
* Added support for record datatypes in Decl'NetCDF
  [#328](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/328)

#### Changed
* Refactored tests in the Decl'HDF5, Decl'NetCDF & Serialize plugins, testing
  now depend on cmake 3.10 & gtest
  [#236](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/236)
* Communicator transtyping in the MPI plugin now uses the specific `Comm_c2f`
  keyworkd for C to Fortran transtyping and `Comm_f2c` for Fortran to C
  transtyping instead of `transtype` for both directions
  [#291](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/291)

#### Fixed
* Support building tests when building in sequential with no MPI present
  (second try)
  [#348](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/348)
* Fixed compilation with GCC-11
  [#353](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/353)
* Correctly detect parallel HDF5 built with cmake
  [#356](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/356)
* Do not depend on Fortran in Decl'HDF5 if testing is not enabled
  [#358](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/358)
* Fixed an issue for `MPI_Comm` transtyping in the MPI plugin when the memory
  layout of the C & Fortran types were the same:
  [#291](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/291)



### [1.2.2] - 2021-07-06

#### Fixed
* When building serial decl_hdf5, enabling tests does not require MPI
  [#348](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/348)



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
* Add more verbose log messages for the `trace` level
  [#306](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/306)
* Support writing and reading data to/from dataset of different dimensions if
  the number of elements matches
  [#308](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/308)

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
* Fix an issue where embedded HDF5 would not compile with parallel support
  [#336](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/336)
* Correctly include MPI in Decl'HDF5 tests and do not run MPI-based tests when
  HDF5 is built in sequential version
  [#274](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/274)



## [1.1.0] - 2021-03-02

### For users

#### Added
* Support for reading and writing attributes of datasets & datagroups in
  Decl'HDF5
* Support getting dataset size before reading it in Decl'HDF5
* support for new operation types in the set-value plugin:
  - `event` to trigger a named event,
  - `release` to release data
* support the trigger of operations on finalization in the set-value plugin



## [1.0.1] - 2021-02-27

### For users

#### Fixed
* Handle the members in a yaml `struct` definition as an ordered mapping instead
  of a plain mapping.
* Fixed errors that would appear when using unsigned values (including Fortran
  logical) in $-expressions.
* Fixed a bug where a null value for a read/write Decl'HDF5 dataset yaml subtree
  would lead to an error.
* Fixed a bug where the order of set/share/expose operations specified in YAML
  would not be respected in the set-value plugin.



## [1.0.0] - 2021-01-28

### For users

#### Added
* Lookup for plugins in paths specified by the `PDI_PLUGIN_PATH` env. variable
* Install plugins as specified by the `INSTALL_PDIPLUGINDIR` cmake variable
* Improved error reporting when loading plugins
* Support for accessing record datatype from Python
* Support loading plugins in standard path relative to LD_LIBRARY_PATH
* Support for accessing members in $-expressions
* Added tests inspired by Parflow that combine Serialize & Decl'HDF5 in a new 
 `tests` sub-project

#### Changed
* Fortran support now requires version 1.0.6 of Zpp minimum
* Python support now depends on python 3
* Require the CONTIGUOUS attribute for all Fortran arrays (including 1D)
* Default logger level for plugin is the logger level of PDI core
* Fortran support now requires python 3.5 with venv support
* Fortran support now requires version 1.0.6 of Zpp minimum
* Bundle version 1.0.6 of Zpp

#### Fixed
* Added the CONTIGUOUS attribute to all Fortran arrays in tests
* Workaround issue #291 in the MPI plugin: MPI transtype fails when
  sizeof(MPI_Comm) == sizeof(MPI_Fint)


### For plugin developers

#### Added
* Expression arithmetic operators
* The `PDI_DEFAULT_PLUGINDIR` cmake variable provides the default plugin
  installation path
* Subaccess methods for array, record and pointer datatypes
* Ref operator[] for getting subref by index and member name
* Documentation of coupling PDI with user application
