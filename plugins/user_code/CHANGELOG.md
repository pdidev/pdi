# Changelog for PDI User-code plugin
All notable changes to the User-code plugin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

### Added

### Changed
* Replaced the astyle based indentation by a clang-format one, fixes
  [#349](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/349)

### Deprecated

### Removed
* Removed support for the PDI Config validator, fixes
  [#458](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/458)

### Fixed

### Security


## [1.7.0] - 2024-07-17

### Fixed
* Fixed compilation with Python version > 3.12


## [1.5.0] - 2022-03-30

### Changed
* Updated dependencies, now require cmake-3.10.


## [1.3.0] - 2021-07-30

### Added
* `logging` spdlog pattern can be set from specification tree
  [#317](https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/317)


## [1.0.0] - 2021-01-28

### Added
* Install in the path specified by the `INSTALL_PDIPLUGINDIR` cmake variable
