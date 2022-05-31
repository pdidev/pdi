# Changelog for PDI Serialize plugin
All notable changes to the Serialize plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [1.5.3] - 2022-05-31

### Fixed
* Fixed a missing header compilation issue latest GCC
  [#426](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/426)


## [1.5.0] - 2022-03-30

### Added
* Support for tuple datatype serialization. It is an array-like structure
  that can store different type elements.
  [#364](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/364)

### Changed
* Updated dependencies, now require cmake-3.10.


## [1.3.0] - 2021-07-30

### Added
* Type attribute support
* `logging` spdlog pattern can be set from specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)

### Changed
* Refactored tests, testing now depend on cmake 3.10 & gtest
  [#236](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/236)


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
