# Changelog for the PDI distribution
All notable changes to the PDI distribution will be documented in this file.

Please note this is the list for the distribution mechanism. The list for each
sub-project is located in the sub-project `CHANGELOG.md` file. For example, the
changes for PDI library proper are in [`pdi/CHANGELOG.md`](pdi/CHANGELOG.md).

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed
* Updated dependencies, the minimum required versions are now: 3.10 for cmake,
  1.6 for FTI, 1.10 for HDF5, 4.6 for NetCDF, 0.4.15 for paraconf, 3.6 for
  Python, 1.7.6 for sionlib, 1.5 for spdlog, and 1.0.15 for zpp.
* Force the `CMAKE_POSITION_INDEPENDENT_CODE` & `BUILD_SHARED_LIBS` to on for
  all sub-projects

### Deprecated

### Removed

### Fixed
* Fix an issue where libYaml is not found correctly when installed in a
  non-standard directory
  [#398](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/398)

### Security


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
