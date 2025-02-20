# Changelog for the PDI distribution
All notable changes to the PDI distribution will be documented in this file.

Please note this is the list for the distribution mechanism. The list for each
sub-project is located in the sub-project `CHANGELOG.md` file. For example, the
changes for PDI library proper are in [`pdi/CHANGELOG.md`](pdi/CHANGELOG.md).

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [1.8.2] - 2025-02-20

### Fixed
* Correct the directory where the vendored HDF5 library is  installed on RHEL,
  fixes [#532](https://github.com/pdidev/pdi/issues/532)


## [1.8.1] - 2025-01-23

### Fixed
* Fix the indentation check script
  [#515](https://github.com/pdidev/pdi/issues/515)


## [1.8.0] - 2024-11-28

### Added
* NetCDF plugin now support the size_of attribute, fixes
  [#446](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/446)
* Deisa plugin for in-situ analysis using Dask

### Changed
* Replaced the astyle based indentation by a clang-format one, fixes
  [#349](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/349)
* Update the version of dependencies according to our policy: oldest supported
  Ubuntu & Fedora, oldstable debian & spack 0.18. The new requirements are:
  CMake 3.16, NetCDF 4.7, Python 3.8, mpi4py 3.0, numpy 1.17, pybind11 2.4
  - do not embed doxygen anymore
  - directly use vendored gtest, benchmark & ZPP
  [#465](https://github.com/pdidev/pdi/issues/465)

### Removed
* Removed deprecated, unmaintained and unsupported plugins and tools for the
  distribution: Decl'SION, FlowVR, FTI and test plugins, as well as the
  PDI Config validator, fixes
  [#458](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/458)

### Fixed
* Support installation with the latest version of python where setuptools is an
  external lib, fixes
  [#457](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/457)


## [1.7.1] - 2024-07-21

### Fixed
* Fix the installation of embedded PyYAML, fixes
  [#455](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/455)


## [1.7.0] - 2024-07-17

### Fixed
* Updated the embedded version of PyYAML to 6.0.1 to fix an incompatibility with
  cython 3, fixes
  [#439](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/439)
* Update PDI tested versions of CMake from 3.10 to 3.25.
  Fixed CMake policy warnings introduced in v3.24 and v3.27.6, fixes
  [#437](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/437)


## [1.6.0] - 2023-03-08

### Changed
* NetCDF 4.6.2 is now required for parallel NetCDF support.
* Updated vendored PyYaml version to 6.0.
* Remove {fmt} as a direct dependency, use that provided by spdlog to fix
  [#431](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/431)
  (reverts
  [#413](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/413))
* Update required version of paraconf to 1.0.0


## [1.5.2] - 2022-04-22

### Fixed
* Fixed an issue where PDI would not build with cmake 3.23+
  [#422](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/422)


## [1.5.0] - 2022-03-30

### Added

* PDI benchmarks using google benchmark
[#341](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/341)

### Changed
* Updated dependencies, the minimum required versions are now: 3.10 for cmake,
  1.6 for FTI, 1.10 for HDF5, 4.6 for NetCDF, 0.4.15 for paraconf, 3.6 for
  Python, 1.7.6 for sionlib, 1.5 for spdlog, and 1.0.15 for zpp.
* Force the `CMAKE_POSITION_INDEPENDENT_CODE` & `BUILD_SHARED_LIBS` to on for
  all sub-projects
* Add {fmt} that is now a direct dependency of PDI to fix
  [#413](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/413)

### Fixed
* Fix an issue where libYaml is not found correctly when installed in a
  non-standard directory
  [#398](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/398)


## [1.4.3] - 2021-11-30

### Fixed
* Fix an issue where HDF5 installed with cmake would not be correctly detected
  [#356](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/346)


## [1.2.1] - 2021-06-18

### Fixed
* Fix an issue where embedded HDF5 would not be installed in the correct path
  [#346](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/346)


## [1.2.0] - 2021-06-15

### Fixed
* Fix an issue where embedded HDF5 would not compile with parallel support
  [#336](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/336)


## [1.0.0] - 2021-01-28

### Added
* A new `tests` sub-project contains tests that depend on multiple plugins

### Changed
* Python support now depends on python 3
* Fortran support now requires python 3.5 with venv support
* Fortran support now requires version 1.0.6 of Zpp minimum
* Bundle version 1.0.6 of Zpp
