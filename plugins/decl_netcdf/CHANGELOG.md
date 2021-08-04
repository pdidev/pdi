# Changelog for PDI Decl'NetCDF plugin
All notable changes to the Decl'NetCDF plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed

### Deprecated

### Removed

### Fixed
* Removed (unsupported by Clang < 11.0) default argument from function parameter
  before parameter pack.
  [#370](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/370)

### Security


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
