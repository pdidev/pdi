# Changelog for PDI Decl'NetCDF plugin
All notable changes to the Decl'NetCDF plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed
* Updated dependencies, now require cmake-3.10, hdf5-1.10.0, and netcdf-4.6.0.

### Deprecated

### Removed

### Fixed
* Fixed an issue where parallel NetCDF would not be detected correctly with
  NetCDF too old and the wrong tests would be run
  [#383](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/383)

### Security


## [1.4.1] - 2021-11-10

### Changed
* updated FindHDF5 to a backport from CMake 3.21.

### Fixed
* fixed a bug where macro definitions imposed by NetCDF would lead to duplicate
  `-D`, hence preventing compilation.


## [1.4.0] - 2021-11-09

### Fixed
* Removed (unsupported by Clang < 11.0) default argument from function parameter
  before parameter pack.
  [#370](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/370)


## [1.3.0] - 2021-07-30

### Added
* Record datatype support
  [#328](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/328)
* `logging` spdlog pattern can be set from specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)

### Changed

* Refactored tests, testing now depend on cmake 3.10 & gtest
  [#236](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/236)


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
